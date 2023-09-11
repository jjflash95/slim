# SLIM

Rust for dummies

### Install

```bash
$ cd project/root
$ cargo build --release
$ export PATH="$(pwd)/target/release/:$PATH"
$ source ~/.bashrc
```

### Usage

to run a file:
```bash
$ echo "print('Hello world')" > hello.sm
$ slim hello.sm
$ Hello world
```
interactive:
```bash
$ slim
```

### Examples
Linked list
```rust
struct List { head }
struct Node { value, next }

impl List {
	fn add(value) {
		curr = self.head
		while curr.next != nil {
			curr = curr.next
		}
		curr.next = new Node { value: value, next: nil }
	}
}

new_list = fn() {
	return new List {
		head: new Node {
			value: 1,
			next: nil
		}
	}
}

list = new_list()
list.add(2)
list.add(3)

print(list)

```