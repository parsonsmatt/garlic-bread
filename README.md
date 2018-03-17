# garlic-bread

A monad transformer for remembering where you've been.

## Example: XML parsing

The initial inspiration for this library came in the form of parsing XML.
Buggy, underspecified, *weird* XML.
I'd write a parser, and it would work OK on the test data, but then we'd release it into production, and suddenly it found parse errors.
These documents were huge, repetitive, deeply nested, and unweildy.

I quickly realized that I needed a way to remember where I've been.
Remembering the tales of *Theseus and the Minotaur* and was *Hansel and Gretel*, I started writing some combinators to remember the path through the XML document.
When a parse failed, I bubbled the breadcrumbs up.

Suddenly, reading the error messages became easy: it told me exactly how to get to the data that failed the test!
