package categorytheory

import categorytheory.core.Applicative.ToApplicativeOps
import categorytheory.core.Functor.ToFunctorOps
import categorytheory.core.Monad.ToMonadOps
import categorytheory.core.Monoid.ToMonoidOps

package object core {

  object ops extends ToMonoidOps
    with ToFunctorOps
    with ToApplicativeOps
    with ToMonadOps

  object implicits extends Implicits
    with FunctorImplicits
    with ApplicativeImplicits
    with MonoidImplicits

}
