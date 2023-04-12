# Immediate goal

Implement compiler to compile following minimal program into bytecode:

Source code:
```
def main = high PA1

// The actual hardware specific library will provide PA1 and an instance of this:
trait DigitalOutput[P]
   def toggle pin:P IO[Unit]
   def high pin:P IO[Unit]
   def low pin:P IO[Unit]
```

This is supposed to set the PA1 pin high.

Note: Program should be terminated by MCUCR=1; SLEEP; JMP -2, so it'll remain in Idle indefinitely when
the code "runs out".

Inside hardware:

```
data Port = PORTA | PORTB | PORTC | PORTD

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

def forever f = f; forever f // ; only available for Monad or Applicative?, so <=> def forever f:F[A] F[B] where F: Applicative

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


TODOs / NOTEs:
- If method name not in the beginning, infer where it is by seeing which of the identifiers is used inside the function!
- Can't have "high" without actually configuring the pin as output.
- ; == >>
- Separate different kinds of I/O, like pin output, pin input, timers, etc. Make the type system infer multiple typeclasses here

