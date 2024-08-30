package dh.lang

trait iifs:
  def iif[A <: Out, B <: Out, Out](cond: Boolean, `then`: => A, `else`: => B): Out =
    if (cond) `then`
    else `else`

  def iif[A <: Out, B <: Out, C <: Out, Out](
      cond: Boolean,
      `then`: => A,
      cond2: => Boolean,
      then2: => B,
      `else`: => C
  ): Out =
    if (cond) `then`
    else if (cond2) then2
    else `else`

  def iif[A <: Out, B <: Out, C <: Out, D <: Out, Out](
      cond: Boolean,
      `then`: => A,
      cond2: => Boolean,
      then2: => B,
      cond3: => Boolean,
      then3: => C,
      `else`: => D
  ): Out =
    if (cond) `then`
    else if (cond2) then2
    else if (cond3) then3
    else `else`

  def iif[A <: Out, B <: Out, C <: Out, D <: Out, E <: Out, Out](
      cond: Boolean,
      `then`: => A,
      cond2: => Boolean,
      then2: => B,
      cond3: => Boolean,
      then3: => C,
      cond4: => Boolean,
      then4: => D,
      `else`: => E
  ): Out =
    if (cond) `then`
    else if (cond2) then2
    else if (cond3) then3
    else if (cond4) then4
    else `else`

  def iif[A <: Out, B <: Out, C <: Out, D <: Out, E <: Out, F <: Out, Out](
      cond: Boolean,
      `then`: => A,
      cond2: => Boolean,
      then2: => B,
      cond3: => Boolean,
      then3: => C,
      cond4: => Boolean,
      then4: => D,
      cond5: => Boolean,
      then5: => E,
      `else`: => F
  ): Out =
    if (cond) `then`
    else if (cond2) then2
    else if (cond3) then3
    else if (cond4) then4
    else if (cond5) then5
    else `else`

  def iif[A <: Out, B <: Out, C <: Out, D <: Out, E <: Out, F <: Out, G <: Out, Out](
      cond: Boolean,
      `then`: => A,
      cond2: => Boolean,
      then2: => B,
      cond3: => Boolean,
      then3: => C,
      cond4: => Boolean,
      then4: => D,
      cond5: => Boolean,
      then5: => E,
      cond6: => Boolean,
      then6: => F,
      `else`: => G
  ): Out =
    if (cond) `then`
    else if (cond2) then2
    else if (cond3) then3
    else if (cond4) then4
    else if (cond5) then5
    else if (cond6) then6
    else `else`

  def iif[A <: Out, B <: Out, C <: Out, D <: Out, E <: Out, F <: Out, G <: Out, H <: Out, Out](
      cond: Boolean,
      `then`: => A,
      cond2: => Boolean,
      then2: => B,
      cond3: => Boolean,
      then3: => C,
      cond4: => Boolean,
      then4: => D,
      cond5: => Boolean,
      then5: => E,
      cond6: => Boolean,
      then6: => F,
      cond7: => Boolean,
      then7: => G,
      `else`: => H
  ): Out =
    if (cond) `then`
    else if (cond2) then2
    else if (cond3) then3
    else if (cond4) then4
    else if (cond5) then5
    else if (cond6) then6
    else if (cond7) then7
    else `else`

  def iif[A <: Out, B <: Out, C <: Out, D <: Out, E <: Out, F <: Out, G <: Out, H <: Out, I <: Out, Out](
      cond: Boolean,
      `then`: => A,
      cond2: => Boolean,
      then2: => B,
      cond3: => Boolean,
      then3: => C,
      cond4: => Boolean,
      then4: => D,
      cond5: => Boolean,
      then5: => E,
      cond6: => Boolean,
      then6: => F,
      cond7: => Boolean,
      then7: => G,
      cond8: => Boolean,
      then8: => H,
      `else`: => I
  ): Out =
    if (cond) `then`
    else if (cond2) then2
    else if (cond3) then3
    else if (cond4) then4
    else if (cond5) then5
    else if (cond6) then6
    else if (cond7) then7
    else if (cond8) then8
    else `else`
