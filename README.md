The Prime F# Code Library [![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/bryanedds/NuGameEngine/blob/master/LICENSE.md) 
=

## Features

- A serialization system based on symbolic expressions with the **Symbol** and related types.
- A purely functional, publisher-neutral event system with **EventSystem** and related.
- The functional-reactive arrow and monad **Observation** and **Chain** built upon said event system.
- A purely functional, dynamic property system called **Xtension**.
- A purely functional random number generator called **Rand**.
- The incredibly valuable **Vsync** monad allowing the same program to be run in parallel or debugged sequentially.
- The fastest-known persistent hash map in F#, **Vmap** - over twice as fast as Map, and 1/3 look-up speed of Dictionary!
- Revolutionary pure-functional wrappers for arbitrary impure objects, **KeyedCache** and **MutantCache**.
- So many extension primitives I couldn't hope to mention them all!

Prime is built with clean and modular **Abstract Data Type** programming style as presented here - https://vimeo.com/128464151

This library makes a good base for non-trivial projects like renderers, game engines, and other real-ass shit that absolutely *needs* to be built with good programming semantics.

## Needs More Cowbell
This project has reached alpha-status, but is rather lacking in tests, having its correctness proven only by its heavy use in the pure functional game engine Nu -  https://github.com/bryanedds/NuGameEngine. More tests are coming along over time, however :)
