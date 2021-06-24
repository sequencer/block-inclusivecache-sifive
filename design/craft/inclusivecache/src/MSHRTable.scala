object MSHRTable {
  object CacheTable extends App {

    val bundleState = (
      Seq(
        A,
        X,
        C) cross Seq(
        PutFullData,
        PutPartialData,
        ArithmeticData,
        LogicalData,
        Get,
        Hint,
        AcquireBlock,
        AcquirePerm,
        Release,
        ReleaseData
      ) cross Seq(
        NtoB,
        NtoT,
        BtoT,
        TtoB,
        TtoN,
        BtoN,
        TtoT,
        BtoB,
        NtoN,
        MIN,
        MAX,
        MINU,
        MAXU,
        ADD,
        XOR,
        OR,
        AND,
        SWAP,
        ReservedParam
      ))
      .par
      .map(v => (v._1._1, v._1._2, v._2))
      .filter {
        case (A, AcquireBlock | AcquirePerm, _: Grow) => true
        case (A, ArithmeticData, _: Arithmetic) => true
        case (A, LogicalData, _: Logic) => true
        case (A, Hint, _: HintParam) => true
        case (A, PutFullData | PutPartialData | Get, ReservedParam) => true
        case (C, Release | ReleaseData, _: Report | _: Shrink) => true
        case (X, ReservedOp, ReservedParam) => true
        case _ => false
      } map { case (c, o, p) => BundleState(c, o, p) }

    case class BundleState(channel: Channel, opCode: OpCode, parameter: Parameter)

    case object A extends Channel

    case object C extends Channel

    case object X extends Channel

    // A
    case object PutFullData extends OpCode

    case object PutPartialData extends OpCode

    case object ArithmeticData extends OpCode

    case object LogicalData extends OpCode

    case object Get extends OpCode

    case object Hint extends OpCode

    case object AcquireBlock extends OpCode

    case object AcquirePerm extends OpCode

    case object ReservedOp extends OpCode

    // C
    case object Release extends OpCode

    case object ReleaseData extends OpCode

    case object NtoB extends Parameter with Grow

    case object NtoT extends Parameter with Grow

    case object BtoT extends Parameter with Grow

    case object TtoB extends Parameter with Shrink

    case object TtoN extends Parameter with Shrink

    case object BtoN extends Parameter with Shrink

    case object TtoT extends Parameter with Report

    case object BtoB extends Parameter with Report

    case object NtoN extends Parameter with Report

    case object PREFETCH_READ extends Parameter with HintParam

    case object PREFETCH_WRITE extends Parameter with HintParam

    case object ReservedParam extends Parameter

    case object MIN extends Parameter with Arithmetic

    case object MAX extends Parameter with Arithmetic

    case object MINU extends Parameter with Arithmetic

    case object MAXU extends Parameter with Arithmetic

    case object ADD extends Parameter with Arithmetic

    case object XOR extends Parameter with Logic

    case object OR extends Parameter with Logic

    case object AND extends Parameter with Logic

    case object SWAP extends Parameter with Logic

    case object HitY extends Hit

    case object HitN extends Hit

    case object DirtyY extends Dirty

    case object DirtyN extends Dirty

    case object Tip extends State

    case object Trunk extends State

    case object Branch extends State

    case object Nothing extends State

    case object ProbeY extends Probe

    case object ProbeN extends Probe

    trait Channel

    trait OpCode

    trait Parameter

    trait Hit

    trait Dirty

    trait State

    trait Probe

    trait Grow

    trait Shrink

    trait Report

    trait HintParam

    trait Arithmetic

    trait Logic

    implicit class Crossable[X](xs: Traversable[X]) {
      def cross[Y](ys: Traversable[Y]) = for {x <- xs; y <- ys} yield (x, y)
    }
  }
}
