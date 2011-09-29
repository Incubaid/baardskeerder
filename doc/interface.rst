Interface
=========
.. code-block:: ocaml

module type DB = sig
  type t
  val set : t -> key -> value -> unit
  
end


now some text



