package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val notA1, notA2, notOutput = new Wire
    inverter(a1, notA1)
    inverter(a2, notA2)
    andGate(notA1, notA2, notOutput)
    inverter(notOutput, output)
  }
  
  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    c match {
      case Nil => andGate(in, in, out(0))
      case head :: tail => {
        val out0, out1, not = new Wire
        andGate(in, head, out0)
        inverter(head, not)
        andGate(in, not, out1)
        val outs = out.length / 2
        demux(out0, tail, out take outs)
        demux(out1, tail, out drop outs)      
      } 
    }
  }  

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }
  
  def orGateExample {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run

    in1.setSignal(false)
    run

    in2.setSignal(false)
    run
  }


  def demuxExample {
    val in, in1, in2, out1, out2, out3, out4 = new Wire

    probe("in", in)

    probe("in1", in1)
    probe("in2", in2)

    probe("out1", out1)
    probe("out2", out2)
    probe("out3", out3)
    probe("out4", out4)

    demux(in, List(in1, in2), List(out1, out2, out3, out4))

    run

    in.setSignal(true)
    run

    in2.setSignal(true)
    run

    in1.setSignal(true)
    run

    in2.setSignal(false)
    run
  }
  
}

object CircuitMain extends App {
//  Circuit.andGateExample
//  Circuit.orGateExample
  println("Demux Gate")
  Circuit.demuxExample
}
