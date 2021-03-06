package com.lightning.walletapp.ln

import com.softwaremill.quicklens._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.ln.PaymentInfo._
import java.util.concurrent.Executors
import fr.acinq.eclair.UInt64

import com.lightning.walletapp.ln.crypto.{Generators, ShaChain, ShaHashesWithIndex, Sphinx}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import com.lightning.walletapp.ln.Helpers.{Closing, Funding}
import fr.acinq.bitcoin.{BinaryData, Satoshi, Transaction}
import com.lightning.walletapp.ln.Tools.{none, runAnd}
import fr.acinq.bitcoin.Crypto.{Point, Scalar}
import scala.util.{Failure, Success}


abstract class Channel extends StateMachine[ChannelData] { me =>
  implicit val context: ExecutionContextExecutor = ExecutionContext fromExecutor Executors.newSingleThreadExecutor
  def apply[T](ex: Commitments => T) = Some(data) collect { case some: HasCommitments => ex apply some.commitments }
  def process(change: Any): Unit = Future(me doProcess change) onFailure { case err => events onException me -> err }
  var listeners: Set[ChannelListener] = _

  private[this] val events = new ChannelListener {
    override def onProcessSuccess = { case ps => for (lst <- listeners if lst.onProcessSuccess isDefinedAt ps) lst onProcessSuccess ps }
    override def onException = { case failure => for (lst <- listeners if lst.onException isDefinedAt failure) lst onException failure }
    override def onBecome = { case transition => for (lst <- listeners if lst.onBecome isDefinedAt transition) lst onBecome transition }

    override def outPaymentAccepted(rd: RoutingData) = for (lst <- listeners) lst outPaymentAccepted rd
    override def fulfillReceived(ok: UpdateFulfillHtlc) = for (lst <- listeners) lst fulfillReceived ok
    override def sentSig(cs: Commitments) = for (lst <- listeners) lst sentSig cs
    override def settled(cs: Commitments) = for (lst <- listeners) lst settled cs
  }

  def SEND(msg: LightningMessage): Unit
  def ASKREFUNDTX(ref: RefundingData): Unit
  def CLOSEANDWATCH(close: ClosingData): Unit
  def STORE(content: HasCommitments): HasCommitments
  def UPDATA(d1: ChannelData): Channel = BECOME(d1, state)
  def BECOME(data1: ChannelData, state1: String) = runAnd(me) {
    // Transition should always be defined before vars are updated
    val trans = Tuple4(me, data1, state, state1)
    super.become(data1, state1)
    events onBecome trans
  }

  def doProcess(change: Any) = {
    Tuple3(data, change, state) match {
      case (InitData(announce), cmd: CMDOpenChannel, WAIT_FOR_INIT) =>
        BECOME(WaitAcceptData(announce, cmd), WAIT_FOR_ACCEPT) SEND OpenChannel(LNParams.chainHash, cmd.tempChanId,
          cmd.realFundingAmountSat, cmd.pushMsat, cmd.localParams.dustLimit.amount, cmd.localParams.maxHtlcValueInFlightMsat,
          cmd.localParams.channelReserveSat, LNParams.minHtlcValue.amount, cmd.initialFeeratePerKw, cmd.localParams.toSelfDelay,
          cmd.localParams.maxAcceptedHtlcs, cmd.localParams.fundingPrivKey.publicKey, cmd.localParams.revocationBasepoint,
          cmd.localParams.paymentBasepoint, cmd.localParams.delayedPaymentBasepoint, cmd.localParams.htlcBasepoint,
          Generators.perCommitPoint(cmd.localParams.shaSeed, index = 0L), channelFlags = 0.toByte)


      case (wait @ WaitAcceptData(announce, cmd), accept: AcceptChannel, WAIT_FOR_ACCEPT) if accept.temporaryChannelId == cmd.tempChanId =>
        if (accept.dustLimitSatoshis > cmd.localParams.channelReserveSat) throw new LightningException("Our channel reserve is less than their dust")
        if (accept.channelReserveSatoshis > cmd.realFundingAmountSat / 10) throw new LightningException("Their proposed reserve is too high")
        if (UInt64(10000L) > accept.maxHtlcValueInFlightMsat) throw new LightningException("Their maxHtlcValueInFlightMsat is too low")
        if (accept.toSelfDelay > cmd.localParams.toSelfDelay) throw new LightningException("Their toSelfDelay is too high")
        if (accept.dustLimitSatoshis < 546L) throw new LightningException("Their on-chain dust limit is too low")
        if (accept.maxAcceptedHtlcs > 483) throw new LightningException("They can accept too many payments")
        if (accept.htlcMinimumMsat > 10000L) throw new LightningException("Their htlcMinimumMsat too high")
        if (accept.maxAcceptedHtlcs < 1) throw new LightningException("They can accept too few payments")
        if (accept.minimumDepth > 9L) throw new LightningException("Their minimumDepth is too high")
        BECOME(WaitFundingData(announce, cmd, accept), WAIT_FOR_FUNDING)


      case (WaitFundingData(announce, cmd, accept), CMDFunding(fundTx), WAIT_FOR_FUNDING) =>
        // They have accepted our proposal, let them sign a first commit so we can broadcast a funding
        if (fundTx.txOut(cmd.outIndex).amount.amount != cmd.realFundingAmountSat) throw new LightningException
        val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) = Funding.makeFirstFunderCommitTxs(cmd, accept,
          fundTx.hash, fundingTxOutputIndex = cmd.outIndex, remoteFirstPoint = accept.firstPerCommitmentPoint)

        val localSigOfRemoteTx = Scripts.sign(remoteCommitTx, cmd.localParams.fundingPrivKey)
        val fundingCreated = FundingCreated(cmd.tempChanId, fundTx.hash, cmd.outIndex, localSigOfRemoteTx)
        val firstRemoteCommit = RemoteCommit(0L, remoteSpec, remoteCommitTx.tx.txid, accept.firstPerCommitmentPoint)
        BECOME(WaitFundingSignedData(announce, cmd.localParams, Tools.toLongId(fundTx.hash, cmd.outIndex), accept, fundTx,
          localSpec, localCommitTx, firstRemoteCommit), WAIT_FUNDING_SIGNED) SEND fundingCreated


      // They have signed our first commit, we can broadcast a funding tx
      case (wait: WaitFundingSignedData, remote: FundingSigned, WAIT_FUNDING_SIGNED) =>
        val signedLocalCommitTx = Scripts.addSigs(wait.localCommitTx, wait.localParams.fundingPrivKey.publicKey,
          wait.remoteParams.fundingPubkey, Scripts.sign(wait.localCommitTx, wait.localParams.fundingPrivKey), remote.signature)

        if (Scripts.checkValid(signedLocalCommitTx).isFailure) BECOME(wait, CLOSING) else {
          val localCommit = LocalCommit(0L, wait.localSpec, htlcTxsAndSigs = Nil, signedLocalCommitTx)
          val commits = Commitments(wait.localParams, wait.remoteParams, localCommit, wait.remoteCommit,
            localChanges = Changes(proposed = Vector.empty, signed = Vector.empty, acked = Vector.empty),
            remoteChanges = Changes(proposed = Vector.empty, signed = Vector.empty, acked = Vector.empty),
            localNextHtlcId = 0L, remoteNextHtlcId = 0L, Right(Tools.randomPrivKey.toPoint),
            wait.localCommitTx.input, ShaHashesWithIndex(Map.empty, None), wait.channelId)

          BECOME(WaitFundingDoneData(wait.announce, None, None,
            wait.fundingTx, commits), WAIT_FUNDING_DONE)
        }


      // FUNDING TX IS BROADCASTED AT THIS POINT


      case (wait: WaitFundingDoneData, their: FundingLocked, WAIT_FUNDING_DONE) =>
        // No need to store their FundingLocked because it gets re-sent on reconnect
        if (wait.our.isEmpty) me UPDATA wait.copy(their = Some apply their)
        else becomeOpen(wait, their)


      case (wait: WaitFundingDoneData, CMDConfirmed(fundTx), WAIT_FUNDING_DONE)
        // GUARD: this funding transaction blongs to this exact channel
        if wait.fundingTx.txid == fundTx.txid =>

        // Create and store our FundingLocked
        val our = makeFundingLocked(wait.commitments)
        val wait1 = me STORE wait.copy(our = Some apply our)
        if (wait.their.isEmpty) me UPDATA wait1 SEND our
        else becomeOpen(wait, wait.their.get) SEND our


      // OPEN MODE


      case (norm: NormalData, hop: Hop, OPEN | OFFLINE) =>
        // Got either an empty Hop with shortChannelId or a final one
        // do not trigger listeners and silently update a current state
        val d1 = norm.modify(_.commitments.extraHop) setTo Some(hop)
        data = me STORE d1


      case (norm: NormalData, add: UpdateAddHtlc, OPEN) =>
        // Got new incoming HTLC so put it to changes for now
        val c1 = Commitments.receiveAdd(norm.commitments, add)
        me UPDATA norm.copy(commitments = c1)


      case (norm: NormalData, fulfill: UpdateFulfillHtlc, OPEN) =>
        // Got a fulfill for an outgoing HTLC we have sent them earlier
        val c1 = Commitments.receiveFulfill(norm.commitments, fulfill)
        me UPDATA norm.copy(commitments = c1)
        events fulfillReceived fulfill


      case (norm: NormalData, fail: UpdateFailHtlc, OPEN) =>
        // Got a failure for an outgoing HTLC we sent earlier
        val c1 = Commitments.receiveFail(norm.commitments, fail)
        me UPDATA norm.copy(commitments = c1)


      case (norm: NormalData, fail: UpdateFailMalformedHtlc, OPEN) =>
        // Got 'malformed' failure for an outgoing HTLC we sent earlier
        val c1 = Commitments.receiveFailMalformed(norm.commitments, fail)
        me UPDATA norm.copy(commitments = c1)


      // We can send a new HTLC when channel is both operational and online
      case (norm: NormalData, rd: RoutingData, OPEN) if isOperational(me) =>
        val c1 \ updateAddHtlc = Commitments.sendAdd(norm.commitments, rd)
        me UPDATA norm.copy(commitments = c1) SEND updateAddHtlc
        events outPaymentAccepted rd
        doProcess(CMDProceed)


      // We're fulfilling an HTLC we got from them earlier
      // this is a special case where we don't throw if cross signed HTLC is not found
      // it may happen when we have already fulfilled it just before connection got lost
      case (norm @ NormalData(_, commitments, _, _), cmd: CMDFulfillHtlc, OPEN) => for {
        add <- Commitments.getHtlcCrossSigned(commitments, incomingRelativeToLocal = true, cmd.id)
        updateFulfillHtlc = UpdateFulfillHtlc(commitments.channelId, cmd.id, cmd.preimage)

        if updateFulfillHtlc.paymentHash == add.paymentHash
        c1 = Commitments.addLocalProposal(commitments, updateFulfillHtlc)
      } me UPDATA norm.copy(commitments = c1) SEND updateFulfillHtlc


      // Failing an HTLC we got earlier
      case (norm @ NormalData(_, commitments, _, _), cmd: CMDFailHtlc, OPEN) =>
        val c1 \ updateFailHtlc = Commitments.sendFail(commitments, cmd)
        me UPDATA norm.copy(commitments = c1) SEND updateFailHtlc


      case (norm @ NormalData(_, commitments, _, _), cmd: CMDFailMalformedHtlc, OPEN) =>
        val c1 \ updateFailMalformedHtlс = Commitments.sendFailMalformed(commitments, cmd)
        me UPDATA norm.copy(commitments = c1) SEND updateFailMalformedHtlс


      // Fail or fulfill incoming HTLCs
      case (norm: NormalData, CMDHTLCProcess, OPEN) =>
        for (Htlc(false, add) <- norm.commitments.remoteCommit.spec.htlcs)
          me doProcess resolveHtlc(LNParams.nodePrivateKey, add, LNParams.bag)

        // And sign once done
        doProcess(CMDProceed)


      case (norm: NormalData, CMDProceed, OPEN)
        // Only if we have a point and something to sign
        if norm.commitments.remoteNextCommitInfo.isRight &&
          (norm.commitments.localChanges.proposed.nonEmpty ||
          norm.commitments.remoteChanges.acked.nonEmpty) =>

        // Propose new remote commit via commit tx sig
        val nextRemotePoint = norm.commitments.remoteNextCommitInfo.right.get
        val c1 \ commitSig = Commitments.sendCommit(norm.commitments, nextRemotePoint)
        val d1 = me STORE norm.copy(commitments = c1)
        me UPDATA d1 SEND commitSig
        events sentSig c1


      case (norm: NormalData, sig: CommitSig, OPEN) =>
        // We received a commit sig from them, now we can update our local commit
        val c1 \ revokeAndAck = Commitments.receiveCommit(norm.commitments, sig)
        val d1 = me STORE norm.copy(commitments = c1)
        me UPDATA d1 SEND revokeAndAck
        // Clear remote commit first
        doProcess(CMDProceed)
        events settled c1


      case (norm: NormalData, rev: RevokeAndAck, OPEN) =>
        // We received a revocation because we sent a commit sig
        val c1 = Commitments.receiveRevocation(norm.commitments, rev)
        val d1 = me STORE norm.copy(commitments = c1)
        me UPDATA d1 doProcess CMDHTLCProcess


      case (norm: NormalData, CMDFeerate(satPerKw), OPEN) =>
        val localCommitFee = norm.commitments.localCommit.spec.feeratePerKw
        val shouldUpdate = LNParams.shouldUpdateFee(satPerKw, localCommitFee)

        if (shouldUpdate) Commitments.sendFee(norm.commitments, satPerKw) foreach { case c1 \ msg =>
          // We send a fee update if current chan unspendable reserve + commitTx fee can afford it
          // otherwise we fail silently in hope that fee will drop or we will receive a payment
          me UPDATA norm.copy(commitments = c1) SEND msg
          doProcess(CMDProceed)
        }


      case (norm: NormalData, CMDBestHeight(height), OPEN | OFFLINE)
        if Commitments.hasExpiredHtlcs(norm.commitments, height) =>
        startLocalClose(norm)


      // SHUTDOWN in WAIT_FUNDING_DONE and OPEN


      case (wait: WaitFundingDoneData, CMDShutdown(scriptPubKey), WAIT_FUNDING_DONE) =>
        // We have decided to cooperatively close our channel before it has reached a min depth
        val finalKey = scriptPubKey getOrElse wait.commitments.localParams.defaultFinalScriptPubKey
        startShutdown(NormalData(wait.announce, wait.commitments), finalKey)


      case (wait: WaitFundingDoneData, remote: Shutdown, WAIT_FUNDING_DONE) =>
        // They have decided to close our channel before it reached a min depth

        val norm = NormalData(wait.announce, wait.commitments)
        val norm1 = norm.modify(_.remoteShutdown) setTo Some(remote)
        // We just got their Shutdown so we add ours and start negotiations
        startShutdown(norm1, wait.commitments.localParams.defaultFinalScriptPubKey)
        doProcess(CMDProceed)


      case (norm @ NormalData(_, commitments, our, their), CMDShutdown(scriptPubKey), OPEN) =>
        // We may have unsigned outgoing HTLCs or already have tried to close this channel cooperatively
        val nope = our.isDefined | their.isDefined | Commitments.localHasUnsignedOutgoing(commitments)
        val finalKey = scriptPubKey getOrElse commitments.localParams.defaultFinalScriptPubKey
        if (nope) startLocalClose(norm) else startShutdown(norm, finalKey)


      case (norm @ NormalData(_, commitments, _, None), remote: Shutdown, OPEN) =>
        // Either they initiate a shutdown or respond to the one we have sent

        val d1 = norm.modify(_.remoteShutdown) setTo Some(remote)
        val nope = Commitments.remoteHasUnsignedOutgoing(commitments)
        // Can't close cooperatively if they have unsigned outgoing HTLCs
        // we should clear our unsigned outgoing HTLCs and then start a shutdown
        if (nope) startLocalClose(norm) else me UPDATA d1 doProcess CMDProceed


      case (norm @ NormalData(_, commitments, None, their), CMDProceed, OPEN)
        // GUARD: we have previously received their Shutdown
        if inFlightHtlcs(me).isEmpty && their.isDefined =>

        // As a result we issued a CMDProceed and are getting it here now
        // which means we do not have any HTLCs in-flight so can negotiatiate
        startShutdown(norm, commitments.localParams.defaultFinalScriptPubKey)
        doProcess(CMDProceed)


      case (NormalData(announce, commitments, our, their), CMDProceed, OPEN)
        // GUARD: got both shutdowns without HTLCs in-flight so can negotiate
        if inFlightHtlcs(me).isEmpty && our.isDefined && their.isDefined =>

        val firstProposed = Closing.makeFirstClosing(commitments, our.get.scriptPubKey, their.get.scriptPubKey)
        val neg = NegotiationsData(announce, commitments, our.get, their.get, firstProposed :: Nil)
        BECOME(me STORE neg, NEGOTIATIONS) SEND firstProposed.localClosingSigned


      // SYNC and REFUNDING MODE


      case (ref: RefundingData, cr: ChannelReestablish, REFUNDING) =>
        cr.myCurrentPerCommitmentPoint -> ref.remoteLatestPoint match {
          case _ \ Some(ourSavedPoint) => storeRefund(ref, ourSavedPoint)
          case Some(theirPoint) \ _ => storeRefund(ref, theirPoint)
          case _ =>
        }

      case (norm: NormalData, cr: ChannelReestablish, OFFLINE)
        // GUARD: we have started in NORMAL state but their nextRemoteRevocationNumber is too far away
        if norm.commitments.localCommit.index < cr.nextRemoteRevocationNumber && cr.myCurrentPerCommitmentPoint.isDefined =>
        val secret = Generators.perCommitSecret(norm.commitments.localParams.shaSeed, cr.nextRemoteRevocationNumber - 1)
        if (cr.yourLastPerCommitmentSecret contains secret) storeRefund(norm, cr.myCurrentPerCommitmentPoint.get)
        else throw new LightningException


      case (norm: NormalData, cr: ChannelReestablish, OFFLINE) =>
        // If next_local_commitment_number is 1 in both the channel_reestablish it sent
        // and received, then the node MUST retransmit funding_locked, otherwise it MUST NOT
        if (cr.nextLocalCommitmentNumber == 1 && norm.commitments.localCommit.index == 0)
          me SEND makeFundingLocked(norm.commitments)

        // First we clean up unacknowledged updates
        val localDelta = norm.commitments.localChanges.proposed collect { case u: UpdateAddHtlc => true }
        val remoteDelta = norm.commitments.remoteChanges.proposed collect { case u: UpdateAddHtlc => true }
        val c1 = norm.commitments.modifyAll(_.localChanges.proposed, _.remoteChanges.proposed).setTo(Vector.empty)
          .modify(_.remoteNextHtlcId).using(_ - remoteDelta.size).modify(_.localNextHtlcId).using(_ - localDelta.size)

        def maybeResendRevocation = if (c1.localCommit.index == cr.nextRemoteRevocationNumber + 1) {
          val localPerCommitmentSecret = Generators.perCommitSecret(c1.localParams.shaSeed, c1.localCommit.index - 1)
          val localNextPerCommitmentPoint = Generators.perCommitPoint(c1.localParams.shaSeed, c1.localCommit.index + 1)
          me SEND RevokeAndAck(channelId = c1.channelId, localPerCommitmentSecret, localNextPerCommitmentPoint)
        } else if (c1.localCommit.index != cr.nextRemoteRevocationNumber) throw new LightningException

        c1.remoteNextCommitInfo match {
          // We had sent a new sig and were waiting for their revocation
          // they didn't receive the new sig because disconnection happened
          // we resend the same updates and sig, also be careful about revocation
          case Left(wait) if wait.nextRemoteCommit.index == cr.nextLocalCommitmentNumber =>
            val revocationWasSentLast = c1.localCommit.index > wait.localCommitIndexSnapshot

            if (!revocationWasSentLast) maybeResendRevocation
            c1.localChanges.signed :+ wait.sent foreach SEND
            if (revocationWasSentLast) maybeResendRevocation

          // We had sent a new sig and were waiting for their revocation, they had received
          // the new sig but their revocation was lost during the disconnection, they'll resend us the revocation
          case Left(wait) if wait.nextRemoteCommit.index + 1 == cr.nextLocalCommitmentNumber => maybeResendRevocation
          case Right(_) if c1.remoteCommit.index + 1 == cr.nextLocalCommitmentNumber => maybeResendRevocation
          case _ => throw new LightningException
        }

        BECOME(norm.copy(commitments = c1), OPEN)
        norm.localShutdown foreach SEND
        doProcess(CMDHTLCProcess)


      // We're exiting a sync state while waiting for their FundingLocked
      case (wait: WaitFundingDoneData, cr: ChannelReestablish, OFFLINE) =>
        BECOME(wait, WAIT_FUNDING_DONE)
        wait.our foreach SEND


      // No in-flight HTLCs here, just proceed with negotiations
      case (neg: NegotiationsData, cr: ChannelReestablish, OFFLINE) =>
        // According to spec we need to re-send a last closing sig here
        val lastSigned = neg.localProposals.head.localClosingSigned
        List(neg.localShutdown, lastSigned) foreach SEND
        BECOME(neg, NEGOTIATIONS) SEND lastSigned


      // SYNC: ONLINE/OFFLINE


      case (some: HasCommitments, CMDOnline, OFFLINE) =>
        val ShaHashesWithIndex(hashes, lastIndex) = some.commitments.remotePerCommitmentSecrets
        val yourLastPerCommitmentSecret = lastIndex.map(ShaChain.moves).flatMap(ShaChain getHash hashes).getOrElse(Sphinx zeroes 32)
        val myCurrentPerCommitmentPoint = Generators.perCommitPoint(some.commitments.localParams.shaSeed, some.commitments.localCommit.index)
        me SEND ChannelReestablish(some.commitments.channelId, some.commitments.localCommit.index + 1, some.commitments.remoteCommit.index,
          Some apply Scalar(yourLastPerCommitmentSecret), Some apply myCurrentPerCommitmentPoint)


      case (wait: WaitFundingDoneData, CMDOffline, WAIT_FUNDING_DONE) => BECOME(wait, OFFLINE)
      case (negs: NegotiationsData, CMDOffline, NEGOTIATIONS) => BECOME(negs, OFFLINE)
      case (norm: NormalData, CMDOffline, OPEN) => BECOME(norm, OFFLINE)


      // NEGOTIATIONS MODE


      case (neg @ NegotiationsData(_, commitments,
        localShutdown, remoteShutdown, ClosingTxProposed(_, localClosingSigned) +: _, _),
        ClosingSigned(channelId, remoteClosingFee, remoteClosingSig), NEGOTIATIONS) =>

        val ClosingTxProposed(closing, closingSigned) = Closing.makeClosing(commitments,
          Satoshi(remoteClosingFee), localShutdown.scriptPubKey, remoteShutdown.scriptPubKey)

        val signedClose = Scripts.addSigs(closing, commitments.localParams.fundingPrivKey.publicKey,
          commitments.remoteParams.fundingPubkey, closingSigned.signature, remoteClosingSig)

        Scripts checkValid signedClose match {
          case Failure(why) => throw new LightningException(why.getMessage)
          case Success(okClose) if remoteClosingFee == localClosingSigned.feeSatoshis =>
            // Our current and their proposed fees are equal for this tx, can broadcast
            startMutualClose(neg, okClose.tx)

          case Success(okClose) =>
            val nextCloseFee = Satoshi(localClosingSigned.feeSatoshis + remoteClosingFee) / 4 * 2
            val nextProposed = Closing.makeClosing(commitments, nextCloseFee, localShutdown.scriptPubKey, remoteShutdown.scriptPubKey)
            if (remoteClosingFee == nextCloseFee.amount) startMutualClose(neg, okClose.tx) SEND nextProposed.localClosingSigned else {
              val d1 = me STORE neg.copy(lastSignedTx = Some(okClose), localProposals = nextProposed +: neg.localProposals)
              me UPDATA d1 SEND nextProposed.localClosingSigned
            }
        }


      // HANDLE FUNDING SPENT


      case (RefundingData(announce, Some(remoteLatestPoint), commitments), CMDSpent(spendTx), REFUNDING)
        // GUARD: we have got a remote commit which we asked them to spend and we have their point
        if spendTx.txIn.exists(input => commitments.commitInput.outPoint == input.outPoint) =>
        val rcp = Closing.claimRemoteMainOutput(commitments, remoteLatestPoint, spendTx)
        val d1 = ClosingData(announce, commitments, refundRemoteCommit = rcp :: Nil)
        BECOME(me STORE d1, CLOSING)


      case (some: HasCommitments, CMDSpent(tx), _)
        // GUARD: tx which spends our funding is broadcasted, must react
        if tx.txIn.exists(input => some.commitments.commitInput.outPoint == input.outPoint) =>
        val revokedOpt = Closing.claimRevokedRemoteCommitTxOutputs(some.commitments, tx, LNParams.bag)
        val nextRemoteCommitEither = some.commitments.remoteNextCommitInfo.left.map(_.nextRemoteCommit)

        Tuple3(revokedOpt, nextRemoteCommitEither, some) match {
          case (_, _, close: ClosingData) if close.refundRemoteCommit.nonEmpty => Tools log s"Existing refund"
          case (_, _, close: ClosingData) if close.mutualClose.exists(_.txid == tx.txid) => Tools log s"Existing mutual $tx"
          case (_, _, close: ClosingData) if close.localCommit.exists(_.commitTx.txid == tx.txid) => Tools log s"Existing local $tx"
          case (_, _, close: ClosingData) if close.localProposals.exists(_.unsignedTx.tx.txid == tx.txid) => startMutualClose(close, tx)
          case (_, _, negs: NegotiationsData) if negs.localProposals.exists(_.unsignedTx.tx.txid == tx.txid) => startMutualClose(negs, tx)
          case (Some(claim), _, closingData: ClosingData) => BECOME(me STORE closingData.modify(_.revokedCommit).using(claim +: _), CLOSING)
          case (Some(claim), _, _) => BECOME(me STORE ClosingData(some.announce, some.commitments, revokedCommit = claim :: Nil), CLOSING)
          case (_, Left(nextRemote), _) if nextRemote.txid == tx.txid => startRemoteNextClose(some, nextRemote)
          case _ if some.commitments.remoteCommit.txid == tx.txid => startRemoteCurrentClose(some)
          case _ => startLocalClose(some)
        }


      // HANDLE INITIALIZATION


      case Tuple3(null, ref: RefundingData, null) =>
        if (ref.remoteLatestPoint.isDefined) ASKREFUNDTX(ref)
        super.become(ref, REFUNDING)

      case (null, close: ClosingData, null) => super.become(close, CLOSING)
      case (null, init: InitData, null) => super.become(init, WAIT_FOR_INIT)
      case (null, wait: WaitFundingDoneData, null) => super.become(wait, OFFLINE)
      case (null, neg: NegotiationsData, null) => super.become(neg, OFFLINE)
      case (null, norm: NormalData, null) => super.become(norm, OFFLINE)


      // ENDING A CHANNEL


      case (some: HasCommitments, err: Error, WAIT_FUNDING_DONE | NEGOTIATIONS | OPEN | OFFLINE) =>
        // REFUNDING is an exception here: no matter what happens we can't spend local in that state
        startLocalClose(some)


      case (some: HasCommitments, CMDShutdown, NEGOTIATIONS | OFFLINE) =>
        // Disregard custom scriptPubKey and always refund to local wallet
        // CMDShutdown in WAIT_FUNDING_DONE and OPEN may be cooperative
        startLocalClose(some)


      case _ =>
    }

    // Change has been successfully processed
    events onProcessSuccess Tuple3(me, data, change)
  }

  private def storeRefund(some: HasCommitments, point: Point) = {
    val msg = "please publish your local commitment" getBytes "UTF-8"
    val ref = RefundingData(some.announce, Some(point), some.commitments)
    BECOME(me STORE ref, REFUNDING) SEND Error(ref.commitments.channelId, msg)
  }

  private def makeFundingLocked(cs: Commitments) = {
    val first = Generators.perCommitPoint(cs.localParams.shaSeed, 1L)
    FundingLocked(cs.channelId, nextPerCommitmentPoint = first)
  }

  private def becomeOpen(wait: WaitFundingDoneData, their: FundingLocked) = {
    val theirFirstPerCommitmentPoint = Right apply their.nextPerCommitmentPoint
    val c1 = wait.commitments.copy(remoteNextCommitInfo = theirFirstPerCommitmentPoint)
    BECOME(me STORE NormalData(wait.announce, c1), OPEN)
  }

  private def startShutdown(norm: NormalData, finalScriptPubKey: BinaryData) = {
    val localShutdownMessage = Shutdown(norm.commitments.channelId, finalScriptPubKey)
    val norm1 = norm.modify(_.localShutdown) setTo Some(localShutdownMessage)
    BECOME(norm1, OPEN) SEND localShutdownMessage
  }

  private def startMutualClose(some: HasCommitments, tx: Transaction) = some match {
    case closingData: ClosingData => BECOME(me STORE closingData.copy(mutualClose = tx +: closingData.mutualClose), CLOSING)
    case neg: NegotiationsData => BECOME(me STORE ClosingData(neg.announce, neg.commitments, neg.localProposals, tx :: Nil), CLOSING)
    case _ => BECOME(me STORE ClosingData(some.announce, some.commitments, Nil, tx :: Nil), CLOSING)
  }

  private def startLocalClose(some: HasCommitments): Unit =
    // Something went wrong and we decided to spend our CURRENT commit transaction
    // BUT if we're at negotiations AND we have a signed mutual closing tx then send it
    Closing.claimCurrentLocalCommitTxOutputs(some.commitments, LNParams.bag) -> some match {
      case (_, neg: NegotiationsData) if neg.lastSignedTx.isDefined => startMutualClose(neg, neg.lastSignedTx.get.tx)
      case (localClaim, closingData: ClosingData) => me CLOSEANDWATCH closingData.copy(localCommit = localClaim :: Nil)
      case (localClaim, _) => me CLOSEANDWATCH ClosingData(some.announce, some.commitments, localCommit = localClaim :: Nil)
    }

  private def startRemoteCurrentClose(some: HasCommitments) =
    // They've decided to spend their CURRENT commit tx, we need to take what's ours
    Closing.claimRemoteCommitTxOutputs(some.commitments, some.commitments.remoteCommit, LNParams.bag) -> some match {
      case (remoteClaim, closingData: ClosingData) => me CLOSEANDWATCH closingData.copy(remoteCommit = remoteClaim :: Nil)
      case (remoteClaim, _) => me CLOSEANDWATCH ClosingData(some.announce, some.commitments, remoteCommit = remoteClaim :: Nil)
    }

  private def startRemoteNextClose(some: HasCommitments, nextRemoteCommit: RemoteCommit) =
    // They've decided to spend their NEXT commit tx, once again we need to take what's ours
    Closing.claimRemoteCommitTxOutputs(some.commitments, nextRemoteCommit, LNParams.bag) -> some match {
      case (remoteClaim, closingData: ClosingData) => me CLOSEANDWATCH closingData.copy(nextRemoteCommit = remoteClaim :: Nil)
      case (remoteClaim, _) => me CLOSEANDWATCH ClosingData(some.announce, some.commitments, nextRemoteCommit = remoteClaim :: Nil)
    }
}

object Channel {
  val WAIT_FOR_INIT = "WAIT-FOR-INIT"
  val WAIT_FOR_ACCEPT = "WAIT-FOR-ACCEPT"
  val WAIT_FOR_FUNDING = "WAIT-FOR-FUNDING"
  val WAIT_FUNDING_SIGNED = "WAIT-FUNDING-SIGNED"
  val WAIT_FUNDING_DONE = "WAIT-FUNDING-DONE"
  val NEGOTIATIONS = "NEGOTIATIONS"
  val OFFLINE = "OFFLINE"
  val OPEN = "OPEN"

  // No tears, only dreams now
  val REFUNDING = "REFUNDING"
  val CLOSING = "CLOSING"

  def estimateCanReceive(chan: Channel) = chan { cs =>
    // Somewhat counterintuitive: localParams.channelReserveSat is THEIR unspendable reseve
    // peer's balance can't go below their channel reserve, commit tx fee is always paid by us
    val canReceive = cs.localCommit.spec.toRemoteMsat - cs.localParams.channelReserveSat * 1000L
    math.min(canReceive, LNParams.maxHtlcValueMsat)
  } getOrElse 0L

  def estimateCanSend(chan: Channel) = chan(_.reducedRemoteState.canSendMsat) getOrElse 0L
  def inFlightHtlcs(chan: Channel) = chan(cs => Commitments.latestRemoteCommit(cs).spec.htlcs) getOrElse Set.empty[Htlc]
  def isOperational(chan: Channel) = chan.data match { case NormalData(_, _, None, None) => true case _ => false }
  def isOpening(chan: Channel) = chan.data match { case _: WaitFundingDoneData => true case _ => false }
  def hasReceivedPayments(chan: Channel) = chan(_.remoteNextHtlcId).exists(_ > 0)

  def channelAndHop(chan: Channel) = for {
    // Make sure this hop is the real one
    Some(extraHop) <- chan(_.extraHop)
    if extraHop.htlcMinimumMsat > 0L
  } yield chan -> Vector(extraHop)
}

trait ChannelListener {
  def nullOnBecome(chan: Channel) = {
    val nullTransition = Tuple4(chan, chan.data, null, chan.state)
    if (onBecome isDefinedAt nullTransition) onBecome(nullTransition)
  }

  type Malfunction = (Channel, Throwable)
  type Incoming = (Channel, ChannelData, Any)
  type Transition = (Channel, ChannelData, String, String)
  def onProcessSuccess: PartialFunction[Incoming, Unit] = none
  def onException: PartialFunction[Malfunction, Unit] = none
  def onBecome: PartialFunction[Transition, Unit] = none

  def fulfillReceived(ok: UpdateFulfillHtlc): Unit = none
  def outPaymentAccepted(rd: RoutingData): Unit = none
  def sentSig(cs: Commitments): Unit = none
  def settled(cs: Commitments): Unit = none
}