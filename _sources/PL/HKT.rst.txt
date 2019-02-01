Higher Kinded Types
==================================

.. code ::

    Identity a = a
    List Int
    Array Int
    Either String Int


Above snippet registers some types like :code:`Identity, List, Int, Array, Either, String`,
where :code:`Int, String` are the concrete types and :code:`List, Array, Either, Identity` are type
constructors. Furthermore, :code:`Either` stands apart from the other 3, for it can be applied twice( 
:code:`Either : (a : Type) -> (b : Type) -> Either a b` ), while
the other 3 could only be applied once.

To describe this internal property of types, a concept called **Kind** has been built and, you can regard a kind as the type of a type.

Each type has a kind ascription:

.. code :: 
    
    (the star * means it's the concrete type)

    Int : *
    
    Identity, Array, List : * -> * 
    
    Either : * -> * -> *