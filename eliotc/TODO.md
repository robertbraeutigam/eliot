# Immediate goal

Implement compiler to compile following minimal program into bytecode:

Source code:
```
main = high PA1

// The actual hardware specific library will provide PA1 and an instance of this:
trait DigitalOutput[P]
   toggle pin:P IO[Unit]
   high pin:P IO[Unit]
   low pin:P IO[Unit]
```

This is supposed to set the PA1 pin high.

Note: Program should be terminated by MCUCR=1; SLEEP; JMP -2, so it'll remain in Idle indefinitely when
the code "runs out".

Inside hardware:

```
type Address = Word

data Port = Port Address

portA Port = Port 0x1011
portB Port = Port 0x1012
portC Port = Port 0x1013
portD Port = Port 0x1014

type Bit = 

data Bit = 0 | 1 | 2 | 3 | 4 | 5 | 7

data AvrPin = AvrPin port:Port bit:Bit

implementation DigitalOutput[AvrPin]
   def toggle pin = toggle pin.port pin.bit
   ...

external toggle pin:Byte pin:Byte
```

## Next Step


```
def main = forever (toggle PA1; sleep 1 sec)

def forever f = f; recurse f // ; only available for Monad or Applicative?, so <=> def forever f:F[A] F[B] where F: Applicative

data Duration = millis: Int // Some Int that desugars to Byte or Word depending on usage?

def n sec = Duration 1000 * n  // Should be internal to duration module.

def x * y = ???

def sleep d = internal sleep d millis ???

def f ";" g = f >>= (_ => g)

def >>= = fmap

```
AVR Specific:


```
def PA1 = Pin PORTA BIT0 // Should be internal to Pins

def PORTA = 
```

TYPESYSTEM:
- Type signature is a function
  prepend a = (List(a), List(a))
  as prepend a = ...

  map a = (List a, List a)   // List a -> List a
  map f as = ...

  type levels ::, so
  
  map = _ -> _ -> Applicative ::
  map a b f = (a -> b) -> f a -> f b ::
  map f as = ...

 -> How to express (_ :a -> a), so a function as argument that can take anything

 -> type aliases become just normal functions!

 So if List Num a, where Num is how long the list is

 append = Numeric               <- can be automatically deduced based on the + sign
 append n = a -> List n a -> List (n+1) a     -- note "a" is "forall" free floating
 append a l = ...

 OR

 append N:Numeric => a:A l:List[N, A] List[N+1, a] = 

 length ?

 map[A, B](as: [A], f: A->B): [B]         <- Seems readable, also this will be the base case

 append[A](l: Seq[A], a: A): Seq[A] = ...
 append[A, N: Numeric](l: Seq[N, A], a: A): Seq[N+1, A] = ...

 regexp[S: String, M: Map[String, Optionality]](s: S): Regexp[calculateRegexpType(M, S)]       <- Can we unify this nicely?
 dynamicRegexp(s: String): Regexp[Anything]

 calculateRegexpType(m: Map[String, Optionality], s: String): Map[String, Optionality] = ???
 
 map                                      <- This style is too unreadable I think
     (a, b)  = ([a], a->b, [b]) ::
     (as, f) = ...

 append                                   <- Lots of junk, need to visually match types, what is the type of the type tuple?
     ()     = (Numeric, * , *)
     (n, a) = (List(n, a), a, List(n+1, a))
     (l, a) = ...

EXAMPLES:
- Complicated functions to generate a pattern of led lights
  -> C compiler would compile everything
  -> Eliot should compile to 4 instructions (uniting switching multiple leds to 1 instruction!)

TODOs / NOTEs:
- If method name not in the beginning, infer where it is by seeing which of the identifiers is used inside the function!
- Can't have "high" without actually configuring the pin as output.
- ; == >>
- Separate different kinds of I/O, like pin output, pin input, timers, etc. Make the type system infer multiple typeclasses here
- Parse expressions with backtracking, trying to avoid currying from bottom up: print "Had " ++ show count ++ " number of tokens" 
  - It has only one valid way to parse (show count) first, then the ++, then print.
- type IO[T] = State[RealWorld, T]
- a . f = f a (object-oriented style)
- tuples === value, so f a b c === f(a, b, c)
- does that mean that data type are just names tuples?
  so: data Something = Something a b c ; f a b c === f something --- without recursion
- Separate public / private stuff with a "where" clause
- Operator precedence -> Only inside one Typeclass, explicitly define which methods before which others (per group? per name? per reference?)

