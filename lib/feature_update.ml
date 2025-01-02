module type FeatureUpdate = sig
  type t
  val compare : t -> t -> int
end

module FeatureUpdate = struct
  type t = {
    title: string;
    content: string;
    timestamp: int;
    documented: bool;
  }

  let compare a b = String.compare a.title b.title
end

module FeatureUpdateSet = Set.Make(FeatureUpdate)