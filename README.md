# binary-heap

Binary heap implementation in Chicken Scheme.

## Documentation

The `binary-heap` library is based on the Ocaml heap implementation by
Jean-Christophe Filliatre.

Heaps are encoded as binary trees that have the heap property, namely
the value of any node is greater or equal than the nodes in its left
and right subtrees.

### Constructors 

A binary-heap object is created by procedure `make-binary-heap`:

<procedure>make-binary-heap:: KEY-COMPARE-PROC -> HEAP</procedure>

where KEY-COMPARE-PROC is a user-supplied function that takes two keys
and returns a negative, positive, or zero number depending on how the
first key compares to the second.


### Predicates

<procedure>heap-empty? :: HEAP -> BOOL</procedure>

returns #t if the heap is empty, #f otherwise

### Accessors

<procedure>heap-get-max :: HEAP -> ELEMENT</procedure>

returns a (key . value) pair for an association in the heap with the
largest key. If the heap is empty, an error is signalled.

<procedure>heap-size :: HEAP -> INT</procedure>

returns the size (the number of associations) in the heap

### Transformers

<procedure>heap-insert :: KEY * VALUE * HEAP -> HEAP</procedure> 

returns a new heap object that contains the given association, while
the original heap object is unmodified.

<procedure>heap-delete-max :: HEAP -> HEAP</procedure>

removes the max key and the corresponding association from the
heap. Returns a (key . value) pair of the removed association. If the
heap is empty, an error is signalled.

### Combinators

<procedure>heap-for-each :: PROC * HEAP -> VOID </procedure> 

applies the given procedure PROC to each (key . value) association of
the heap, from the one with the smallest key all the way to the one
with the max key, in an ascending order of keys.

<procedure>heap-fold :: PROC * INIT * HEAP -> RESULT</procedure> 

given the associations in the heap ordered by the descending order of
keys: `(key-n . value-n) ... (key-2 . value-2) (key-1 . value-1)`
the procedure returns the result of the successive function
applications `(PROC value-1 (PROC value-2 ... (PROC value-n
INITIAL)`.


## Examples


## Version history

- 2.0 : Removed message-based interface in favor of a simple procedural interface
- 1.2 : Test script updated to return proper exit code
- 1.1 : Documentation converted to wiki format
- 1.0 : Initial release

## License

>
> Copyright 2009-2016 Ivan Raikov
> 
>  This program is free software: you can redistribute it and/or modify
>  it under the terms of the GNU General Public License as published by
>  the Free Software Foundation, either version 3 of the License, or (at
>  your option) any later version.
>  
>  This program is distributed in the hope that it will be useful, but
>  WITHOUT ANY WARRANTY; without even the implied warranty of
>  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
>  General Public License for more details.
> 
>  A full copy of the GPL license can be found at
>  <http://www.gnu.org/licenses/>.
