import "triangular"
import "element"

module type symmetric_matrix = {
  type t
  type~ mat [n]

  --| Construct a symmetric matrix from an underlying arr.
  val symmetric [n] : [(n * (n + 1i64)) / 2i64]t -> mat [n]

  --| `idx (i,j) m` produces the element at logical position
  -- `(i,j)` in the symmetric matrix `m`.
  val idx [n] : (i64, i64) -> mat [n] -> t

  -- | Symmetric matrix vector multiplication.
  val smvm [n] : mat [n] -> [n]t -> [n]t
}

module type element_plus_conjugate = {
  type t

  val i64 : i64 -> t
  val * : t -> t -> t
  val + : t -> t -> t
  val - : t -> t -> t
  val < : t -> t -> bool

  --| Complex conjugate.
  val conj : t -> t
}

module mk_symmetric (T: element_plus_conjugate) : symmetric_matrix with t = T.t = {
  type t = T.t

  type~ mat [n] = {dummy: [0][n](), data: [(n * (n + 1i64)) / 2i64]t}

  def symmetric [n] (x: [(n * (n + 1i64)) / 2i64]t) : mat [n] =
    { dummy = []
    , data = x
    }

  def idx [n] (i, j) (A: mat [n]) =
    let (i_n, j_n) = if j > i then (j, i) else (i, j)
    let index = (i64.+) i_n 1 |> (i64.*) i_n |> flip (i64./) 2i64 |> (i64.+) j_n
    in if j > i
       then A.data[index] |> T.conj
       else A.data[index]

  def smvm [n] (A: mat [n]) x =
    map (\i -> map2 (\j x_i -> idx (i, j) A |> (T.*) x_i) (iota n) x |> reduce (T.+) (T.i64 0)) (iota n)
}
