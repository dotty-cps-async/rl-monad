package cps.learning.examples.toycompiler



enum Representation {
  case IntRepr()
  case BoolRepr()
  case AstRepr()
  case FunRepr(arg: Representation, res: Representation)
  case PairRepr(fst: Representation, snd: Representation)
}

trait LweredValue

trait LoweringContext {

}

trait SIRType

trait Generator {

  def lower(sir: SIR)(using LoweringContext): LoweredValue

  def chooseRepresentation(tp: SIRType,
                           inputs : Seq[LoweredValue ])
                             (using LoweringContext): Representation

  chooseRepresentation(tp, inputs) 
  
}

trait Generator1 {

  def lower(sir: SIR)(using LoweringContext): ScoredLogicStream[LoweredValue]
  
  def chooseRepresentation(tp: SIRType,
                           inputs : Seq[LoweredValue ])
                          (using LoweringContext): ScoredLogicStream[Representation[]


}
