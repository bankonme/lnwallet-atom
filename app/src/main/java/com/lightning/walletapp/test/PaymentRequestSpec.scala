package com.lightning.walletapp.test

import com.lightning.walletapp.ln._
import com.lightning.walletapp.ln.wire.ChannelUpdate
import fr.acinq.bitcoin.{BinaryData, Block, Btc, Crypto, MilliBtc, MilliSatoshi, Satoshi}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}

/**
  * Created by anton on 13.07.17.
  */
class PaymentRequestSpec {

  def allTests = {
    import com.lightning.walletapp.ln.PaymentRequest._

    val priv = PrivateKey(BinaryData("e126f68f7eafcc8b74f54d269fe206be715000f94dac067d1c04a8ca3b2db734"), compressed = true)
    val pub = priv.publicKey
    val nodeId = pub
    assert(nodeId == PublicKey(BinaryData("03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad")))

    {
      println("check minimal unit is used")
      assert('p' == Amount.unit(MilliSatoshi(1)))
      assert('p' == Amount.unit(MilliSatoshi(99)))
      assert('n' == Amount.unit(MilliSatoshi(100)))
      assert('p' == Amount.unit(MilliSatoshi(101)))
      assert('n' == Amount.unit(Satoshi(1)))
      assert('u' == Amount.unit(Satoshi(100)))
      assert('n' == Amount.unit(Satoshi(101)))
      assert('u' == Amount.unit(Satoshi(1155400)))
      assert('m' == Amount.unit(MilliBtc(1)))
      assert('m' == Amount.unit(MilliBtc(10)))
      assert('m' == Amount.unit(Btc(1)))
    }

    {
      println("check that we can still decode non-minimal amount encoding")
      assert(Some(MilliSatoshi(100000000)) == Amount.decode("1000u"))
      assert(Some(MilliSatoshi(100000000)) == Amount.decode("1000000n"))
      assert(Some(MilliSatoshi(100000000)) == Amount.decode("1000000000p"))
    }

    {
      println("Please make a donation of any amount using payment_hash 0001020304050607080900010203040506070809000102030405060708090102 to me @03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad")
      val ref = "lnbca1pdw3gtxpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdpl2pkx2ctnv5sxxmmwwd5kgetjypeh2ursdae8g6twvus8g6rfwvs8qun0dfjkxaqru9gnlrrvancfg5thgvkm63cac0rrvgrlrg3kq0q7e70l0vkaymjh567kr5658up4wnvgqc23shpykjvzf850kxl04p7jzksg57zvcqpklufl8"
      val pr = PaymentRequest.read(ref)
      assert(pr.prefix == "lnbca")
      assert(pr.amount.isEmpty)
      assert(pr.paymentHash == BinaryData("0001020304050607080900010203040506070809000102030405060708090102"))
      assert(pr.timestamp == 1525195110L)
      assert(pr.nodeId == PublicKey(BinaryData("03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad")))
      assert(pr.tags.size == 2)
      assert(PaymentRequest.write(pr.sign(priv)) == ref)
    }

    {
      println("Please send $3 for a cup of coffee to the same peer, within 1 minute")
      val ref = "lnbca2500u1pdwjmwgpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5xysxxatsyp3k7enxv4jsfzme726098qkvw2dzxcqxp6wtde5cx3dzt34q5m6fnpc6fgazh094r4x6chh3gtmdwfsww2lf8s5ydzulwysgqkg0d3y2a64vepew5squwyrgl"
      val pr = PaymentRequest.read(ref)
      assert(pr.prefix == "lnbca")
      assert(pr.amount == Some(MilliSatoshi(250000000L)))
      assert(pr.paymentHash == BinaryData("0001020304050607080900010203040506070809000102030405060708090102"))
      assert(pr.timestamp == 1525247432L)
      assert(pr.nodeId == PublicKey(BinaryData("03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad")))
      assert(PaymentRequest.write(pr.sign(priv)) == ref)
    }

    {
      println("The same, on testnet, with a fallback address 2My3DfZgsrAirUsf89rdc631TLqdB6vxGSj")
      val ref = "lntbca20m1pdwjatfpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdzafahx2grsd9jkxefqdanzqcmgda3k7mrpw3jjqcmpddjjcgr0dejjq6trv43hyetpd5sxxmmwv5kzqmmwv5s8q6trddkx2fppj879hue0yrdj3qwacc0teknxp5w4p59lysl04ddc2jpg57aulhj44arxf8slvfz9q0u79yglxxn4u24upr0xj2r00ne93wtqq70awwq0r93r5dtumcvp3wuh63v3jlcp5pje0flcpm8yk49"
      val pr = PaymentRequest.read(ref)
      assert(pr.prefix == "lntbca")
      assert(pr.amount == Some(MilliSatoshi(2000000000L)))
      assert(pr.paymentHash == BinaryData("0001020304050607080900010203040506070809000102030405060708090102"))
      assert(pr.timestamp == 1525249385L)
      assert(pr.nodeId == PublicKey(BinaryData("03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad")))
      assert(pr.tags.collect { case u: UnknownTag => u }.size == 1) // fallback address
      assert(pr.tags.size == 3)
      assert(PaymentRequest.write(pr.sign(priv)) == ref)
    }

    {
      println("On mainnet, with fallback address 2My3DfZgsrAirUsf89rdc631TLqdB6vxGSj with extra routing info to go via nodes 029e03a901b85534ff1e92c43c74431f7ce72046060fcf7a95c37e148f78c77255 then 039e03a901b85534ff1e92c43c74431f7ce72046060fcf7a95c37e148f78c77255")
      val ref = "lnbca20m1pdwj708pp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdzafahx2grsd9jkxefqdanzqcmgda3k7mrpw3jjqcmpddjjcgr0dejjq6trv43hyetpd5sxxmmwv5kzqmmwv5s8q6trddkx2fppj879hue0yrdj3qwacc0teknxp5w4p59lyr9yq20q82gphp2nflc7jtzrcazrra7wwgzxqc8u7754cdlpfrmccae92qqqqqqqqqqqqyqqqqq2qqqqqzcqpspeuqafqxu92d8lr6fvg0r5gv0heeeqgcrqlnm6jhphu9y00rrhy4gqqqqqqqqqqqpqqqqqpgqqqqqtqqxq0xsd753tkksaqxw4r8z6lsngn2s5x68sxgd6pmxcctxjcf69vef4axeauz5qggp39s5afyruj7fny3nvtxqwv85n42acydpwgeeszkcq99udl8"
      val pr = PaymentRequest.read(ref)
      assert(pr.prefix == "lnbca")
      assert(pr.amount == Some(MilliSatoshi(2000000000L)))
      assert(pr.paymentHash == BinaryData("0001020304050607080900010203040506070809000102030405060708090102"))
      assert(pr.timestamp == 1525250535L)
      assert(pr.nodeId == PublicKey(BinaryData("03e7156ae33b0a208d0744199163177e909e80176e55d97a2f221ede0f934dd9ad")))

      assert(pr.routingInfo == Vector(RoutingInfoTag(Vector(
        Hop(PublicKey("029e03a901b85534ff1e92c43c74431f7ce72046060fcf7a95c37e148f78c77255"), 1, 12, 0, 10, 11),
        Hop(PublicKey("039e03a901b85534ff1e92c43c74431f7ce72046060fcf7a95c37e148f78c77255"), 2, 12, 0, 10, 11)))
      ))

      assert(pr.tags.size == 4)
      assert(PaymentRequest.write(pr.sign(priv)) == ref)
    }
  }
}
