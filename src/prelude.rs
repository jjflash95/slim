pub static PRELUDE: &str = "
impl sequence {
    fn contains(i) {
        return for item in self {
            if item == i {
                break true
            }
            false
        }
    }
	fn foreach(f) {
		for item in self {
			item << f(item)
		}
		return self
	}

	fn map(f) {
		out = []
		for item in self {
			out[] = f(item)
		}
		return out
	}

	fn enumerate() {
		out = []
		count = 0
		for item in self {
			out[] = [count, item]
			count = count + 1
		}
		return out
	}

	fn slice(start, end) {
		return slice(self, start, end)
	}

	fn len() {
		return len(self)
	}

	fn extend(list) {
		for item in list {
			self[] = item
		}
		return self
	}

	fn pop() {
		return pop(self)
	}

	fn join(sep) {
		out = \"\"
		for i in self {
			out = concat(out, concat(sep, i))
		}
		return out
	}

	fn head() {
		return self[0]
	}

	fn tail() {
		return self[self.len() - 1]
	}
}

trait Number {
    fn to_string() {
		return to_string(self)
	}

    fn pow(n) {
		c = self
		for i in range(n) {
			c = c * self
		}
		return c
	}
}

impl Number for int
impl Number for float
";
