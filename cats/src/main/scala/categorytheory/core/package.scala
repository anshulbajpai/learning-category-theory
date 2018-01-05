package categorytheory

import categorytheory.core.Applicative.ToApplicativeOps
import categorytheory.core.Cartesian.ToCartesianOps
import categorytheory.core.Functor.ToFunctorOps
import categorytheory.core.Monad.ToMonadOps
import categorytheory.core.Monoid.ToMonoidOps

package object core {

  object ops extends ToMonoidOps
    with ToFunctorOps
    with ToApplicativeOps
    with ToMonadOps
    with ToCartesianOps

  object implicits extends Implicits
    with FunctorImplicits
    with ApplicativeImplicits
    with MonoidImplicits

  type ~[F[_], G[_]] = Transformation[F, G]

  type Id[A] = A
}
