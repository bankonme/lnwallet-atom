package com.lightning.walletapp.ln.wire

import com.lightning.walletapp.ln.wire.LightningMessageCodecs._
import java.net.{Inet4Address, Inet6Address, InetSocketAddress}
import com.lightning.walletapp.ln.{Hop, LightningException}
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi, Satoshi}
import fr.acinq.bitcoin.Crypto.{Point, PublicKey, Scalar}
import com.lightning.walletapp.ln.Tools.fromShortId
import fr.acinq.bitcoin.Crypto
import fr.acinq.eclair.UInt64


trait LightningMessage
trait RoutingMessage extends LightningMessage
trait ChannelSetupMessage extends LightningMessage
trait ChannelMessage extends LightningMessage { val channelId: BinaryData }
case class Init(globalFeatures: BinaryData, localFeatures: BinaryData) extends LightningMessage
case class Ping(pongLength: Int, data: BinaryData) extends LightningMessage
case class Pong(data: BinaryData) extends LightningMessage

// CHANNEL SETUP MESSAGES: open channels never get these

case class OpenChannel(chainHash: BinaryData, temporaryChannelId: BinaryData, fundingSatoshis: Long, pushMsat: Long,
                       dustLimitSatoshis: Long, maxHtlcValueInFlightMsat: UInt64, channelReserveSatoshis: Long, htlcMinimumMsat: Long,
                       feeratePerKw: Long, toSelfDelay: Int, maxAcceptedHtlcs: Int, fundingPubkey: PublicKey, revocationBasepoint: Point,
                       paymentBasepoint: Point, delayedPaymentBasepoint: Point, htlcBasepoint: Point, firstPerCommitmentPoint: Point,
                       channelFlags: Byte) extends ChannelSetupMessage

case class AcceptChannel(temporaryChannelId: BinaryData, dustLimitSatoshis: Long, maxHtlcValueInFlightMsat: UInt64,
                         channelReserveSatoshis: Long, htlcMinimumMsat: Long, minimumDepth: Long, toSelfDelay: Int, maxAcceptedHtlcs: Int,
                         fundingPubkey: PublicKey, revocationBasepoint: Point, paymentBasepoint: Point, delayedPaymentBasepoint: Point,
                         htlcBasepoint: Point, firstPerCommitmentPoint: Point) extends ChannelSetupMessage {

  lazy val dustLimitSat = Satoshi(dustLimitSatoshis)
}

case class FundingCreated(temporaryChannelId: BinaryData, fundingTxid: BinaryData,
                          fundingOutputIndex: Int, signature: BinaryData) extends ChannelSetupMessage

case class FundingSigned(channelId: BinaryData, signature: BinaryData) extends ChannelSetupMessage

// CHANNEL MESSAGES

case class FundingLocked(channelId: BinaryData, nextPerCommitmentPoint: Point) extends ChannelMessage
case class ClosingSigned(channelId: BinaryData, feeSatoshis: Long, signature: BinaryData) extends ChannelMessage
case class Shutdown(channelId: BinaryData, scriptPubKey: BinaryData) extends ChannelMessage

case class UpdateAddHtlc(channelId: BinaryData, id: Long,
                         amountMsat: Long, paymentHash: BinaryData, expiry: Long,
                         onionRoutingPacket: BinaryData) extends ChannelMessage {

  lazy val hash160 = Crypto ripemd160 paymentHash
  val amount = MilliSatoshi(amountMsat)
}

case class UpdateFailHtlc(channelId: BinaryData, id: Long, reason: BinaryData) extends ChannelMessage
case class UpdateFailMalformedHtlc(channelId: BinaryData, id: Long, onionHash: BinaryData, failureCode: Int) extends ChannelMessage
case class UpdateFulfillHtlc(channelId: BinaryData, id: Long, paymentPreimage: BinaryData) extends ChannelMessage {

  val paymentHash = Crypto sha256 paymentPreimage.data
}

case class UpdateFee(channelId: BinaryData, feeratePerKw: Long) extends ChannelMessage
case class CommitSig(channelId: BinaryData, signature: BinaryData, htlcSignatures: List[BinaryData] = Nil) extends ChannelMessage
case class RevokeAndAck(channelId: BinaryData, perCommitmentSecret: Scalar, nextPerCommitmentPoint: Point) extends ChannelMessage

case class Error(channelId: BinaryData, data: BinaryData) extends ChannelMessage {
  // Error from remote peer means we need to close a channel, may contain some details

  def exception = {
    val text = new String(data, "UTF-8")
    if (text.isEmpty) new LightningException("Error from remote peer")
    else new LightningException(s"Error from remote peer: $text")
  }
}

case class ChannelReestablish(channelId: BinaryData, nextLocalCommitmentNumber: Long,
                              nextRemoteRevocationNumber: Long, yourLastPerCommitmentSecret: Option[Scalar],
                              myCurrentPerCommitmentPoint: Option[Point] = None) extends ChannelMessage

// ROUTING MESSAGES: open channels never get these except for ChannelUpdate

case class AnnouncementSignatures(channelId: BinaryData, shortChannelId: Long, nodeSignature: BinaryData,
                                  bitcoinSignature: BinaryData) extends RoutingMessage

case class ChannelAnnouncement(nodeSignature1: BinaryData, nodeSignature2: BinaryData, bitcoinSignature1: BinaryData,
                               bitcoinSignature2: BinaryData, features: BinaryData, chainHash: BinaryData, shortChannelId: Long,
                               nodeId1: PublicKey, nodeId2: PublicKey, bitcoinKey1: PublicKey,
                               bitcoinKey2: PublicKey) extends RoutingMessage {

  val (blockHeight, txIndex, outputIndex) = fromShortId(shortChannelId)
  lazy val nodes = Set(nodeId1, nodeId2)
}

case class ChannelUpdate(signature: BinaryData, chainHash: BinaryData, shortChannelId: Long,
                         timestamp: Long, flags: BinaryData, cltvExpiryDelta: Int, htlcMinimumMsat: Long,
                         feeBaseMsat: Long, feeProportionalMillionths: Long) extends RoutingMessage {

  lazy val feeEstimate = (feeBaseMsat + feeProportionalMillionths * 10).toDouble
  def toHop(nodeId: PublicKey) = Hop(nodeId, shortChannelId, cltvExpiryDelta,
    htlcMinimumMsat, feeBaseMsat, feeProportionalMillionths)
}

case class NodeAnnouncement(signature: BinaryData,
                            features: BinaryData, timestamp: Long,
                            nodeId: PublicKey, rgbColor: RGB, alias: String,
                            addresses: NodeAddressList) extends RoutingMessage {

  lazy val socketAddresses = addresses collect {
    case IPv4(addr, port) => new InetSocketAddress(addr, port)
    case IPv6(addr, port) => new InetSocketAddress(addr, port)
  }

  val identifier = (alias + nodeId.toString).toLowerCase
}

sealed trait NodeAddress
case object Padding extends NodeAddress

case object NodeAddress {
  def apply(isa: InetSocketAddress) = isa.getAddress match {
    case inet4Address: Inet4Address => IPv4(inet4Address, isa.getPort)
    case inet6Address: Inet6Address => IPv6(inet6Address, isa.getPort)
    case otherwise => throw new LightningException(otherwise.toString)
  }
}

case class IPv4(ipv4: Inet4Address, port: Int) extends NodeAddress
case class IPv6(ipv6: Inet6Address, port: Int) extends NodeAddress

case class Tor2(tor2: BinaryData, port: Int) extends NodeAddress {
  require(tor2.size == 10, "Invalid Tor2 address length, should be 10")
}

case class Tor3(tor3: BinaryData, port: Int) extends NodeAddress {
  require(tor3.size == 35, "Invalid Tor2 address length, should be 35")
}

// Not in a spec
case class OutRequest(sat: Long, badNodes: Set[String], badChans: Set[Long], from: Set[String], to: String)
case class WalletZygote(v: Int, db: BinaryData, wallet: BinaryData, chain: BinaryData)
case class AESZygote(v: Int, iv: BinaryData, ciphertext: BinaryData)