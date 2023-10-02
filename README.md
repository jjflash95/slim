# SLIM

An interpreter built and inspired in pure rust.

Working [examples](https://github.com/jjflash95/slim-examples)

### Build & Install

assuming you already have rust installed
```bash
$ cd project/root
$ cargo build --release
$ export PATH="$(pwd)/target/release/:$PATH"
$ source ~/.bashrc
$ slim
> print("Hello World")
Hello World
```

### Usage
```bash
$ echo "print('Hello world')" > hello.sm
$ slim hello.sm
$ Hello world
```

### Examples
Cons list
```rust
struct Cons { head }
struct Node { value, next }

impl Cons {
    add(value) {
        curr = self.head
        return loop {
            if curr.next == nil {
                curr.next = new Node { value: value, next: nil }
                break self
            }
            curr = curr.next
        }
    }
}

list = fn() {
    return new Cons {
        head: new Node {
            value: 1,
            next: nil
        }
    }
}

print(
    list().add(2).add(3).add(4)
)
```
