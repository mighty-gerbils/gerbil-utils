# Prototype Object Orientation in Gerbil Scheme

This directory implements POO, a Prototype Object System
with a pure lazy functional interface.
Its semantics is very close to the object system of the
[Nix Expression Language](https://nixos.wiki/wiki/Nix_Expression_Language)
(as defined as a library in `nixpkgs/lib/fixed-points.nix`),
itself essentially identical to that builtin to [Jsonnet](https://jsonnet.org/).
Another influence of note is the [Slate language](https://github.com/briantrice/slate-language).

## Semantic Concepts underlying POO

In POO, an *object*, or *poo*, embodies two different but related concepts:
a list of *prototypes*, and an *instance*.

An *instance* is conceptually a mapping from *slot names* to values.
Slot names are typically symbol or string constants.
Values can be arbitrary language values.
However, to account for computation of slots via object inheritance,
this mapping is organized in *layers*, each layer being mapping from slot names to values,
as computed by super-objects that the current object inherits from.

A *prototype* is an incremental description of how the slots of an object can be computed,
given (a) a record of the values of the slots of the object, i.e. the instance itself, and
(b) a record of the values inherited by the object from its "super-object" via any parent prototype,
i.e. the "super-instance" made of the super-layers beyond the current one.
Prototypes can be combined using function `mix-poo`, which chains computations of slots:
the closest prototype in the list defines the computation,
which may use other slots of the instance, or rely on the values computed by
super-prototypes being inherited from.

To any given list of prototypes, we can uniquely associate an instance,
being the fixed-point of the combined computation specified by the prototype.
An *object*, or *poo*, is the datum of a list of prototypes, and of
its lazily computed fixed-point instance with its lazily computed slots.

Semi-formally, we have the following type definitions:
```
type Layer = (Cons (Map Any <-- Symbol) (Fn Any <-- Symbol))
type Instance = ('Instance (List Layer))
type Prototype = (Map (Fn Any <-- Instance Instance) <-- Symbol)
type Object = Prototype * Instance
```

## Practical considerations

An `Instance` contains a list of layers.
The first layer, also known as base layer is special, and
contains all the known values of slots for the object.
The other layers only contain inherited slots for each notional super-instance of the object.
When looking an an instance from the outside, you may only see values from the base layers;
the values in the other layers are only used during initialization;
but since the initialization is lazy, these layers may survive indefinitely with the instance
as the current language implementation isn't trying to be clever and determine when initialization is over.

In a future implementation, the base layer could be implemented differently,
with support for additional optimizations.


## API

To be documented...
