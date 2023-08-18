mod tests {
    use std::collections::HashMap;

    use crate::runtime::context::MutRef;
    use crate::runtime::Value;
    use crate::runtime::{self, EResult};
    use crate::{parser::parse, runtime::context::RuntimeContext};

    fn get_last_eval(mut input: &str) -> EResult {
        let ctx = RuntimeContext::default();
        let mut r = Ok(Value::Nil);
        loop {
            match parse(input) {
                Ok((i, ast)) => {
                    r = runtime::eval(ast, ctx.mut_ref());
                    input = i;
                }
                Err(_) => {
                    break;
                }
            };
        }

        r
    }

    #[test]
    fn test_operations() {
        let input = "1 + 2";
        let ctx = RuntimeContext::default();
        let ast = parse(input).unwrap().1;
        let r = runtime::eval(ast, ctx);
        assert_eq!(r, Ok(Value::Int(3)));

        let input = "1.0 + 2.5";
        let ctx = RuntimeContext::default();
        let ast = parse(input).unwrap().1;
        let r = runtime::eval(ast, ctx);
        assert_eq!(r, Ok(Value::Float(3.5)));

        let input = "10.0 + 3 * 10";
        let ctx = RuntimeContext::default();
        let ast = parse(input).unwrap().1;
        let r = runtime::eval(ast, ctx);
        assert_eq!(r, Ok(Value::Float(40.0)));

        let input = "10.0 + 3 * 10 + 2";
        let ctx = RuntimeContext::default();
        let ast = parse(input).unwrap().1;
        let r = runtime::eval(ast, ctx);
        assert_eq!(r, Ok(Value::Float(42.0)));

        let input = "66 / 3";
        let ctx = RuntimeContext::default();
        let ast = parse(input).unwrap().1;
        let r = runtime::eval(ast, ctx);
        assert_eq!(r, Ok(Value::Float(22.0)));

        let input = "66 / -3";
        let ctx = RuntimeContext::default();
        let ast = parse(input).unwrap().1;
        let r = runtime::eval(ast, ctx);
        assert_eq!(r, Ok(Value::Float(-22.0)));
    }

    #[test]
    fn test_sequences() {
        let input = "[1, 2, 3]";
        let ctx = RuntimeContext::default();
        let ast = parse(input).unwrap().1;
        let r = runtime::eval(ast, ctx);
        assert_eq!(
            r,
            Ok(Value::Sequence(vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(3)
            ]))
        );

        let input = "
            x = [1, 2, 3]
            x[0] = 100
            x
        ";

        let r = get_last_eval(input);

        assert_eq!(
            r,
            Ok(Value::Sequence(vec![
                Value::Int(100),
                Value::Int(2),
                Value::Int(3)
            ]))
        );

        let input = "
            x = [1, 2, 3]
            x[0] = x[0] * 100
            x[1] = x[1] * 100
            x[2] = x[2] * 100
            x
        ";

        let r = get_last_eval(input);

        assert_eq!(
            r,
            Ok(Value::Sequence(vec![
                Value::Int(100),
                Value::Int(200),
                Value::Int(300)
            ]))
        );

        let input = "
            x = [1, 2, 3]
            index = 1
            x[index] = 999
            x
        ";

        let r = get_last_eval(input);

        assert_eq!(
            r,
            Ok(Value::Sequence(vec![
                Value::Int(1),
                Value::Int(999),
                Value::Int(3)
            ]))
        );
    }

    #[test]
    fn test_collection() {
        let input = "{a: 1, b: 2, c: 3}";
        let ctx = RuntimeContext::default();
        let ast = parse(input).unwrap().1;
        let r = runtime::eval(ast, ctx);
        assert_eq!(
            r,
            Ok(Value::Collection(HashMap::from([
                ("a".to_string(), Value::Int(1)),
                ("b".to_string(), Value::Int(2)),
                ("c".to_string(), Value::Int(3))
            ])))
        );

        let input = "
            x = {a: 1, b: 2, c: 3}
            x.a = 100
            x
        ";

        let r = get_last_eval(input);

        assert_eq!(
            r,
            Ok(Value::Collection(HashMap::from([
                ("a".to_string(), Value::Int(100)),
                ("b".to_string(), Value::Int(2)),
                ("c".to_string(), Value::Int(3))
            ])))
        );

        let input = "
            x = {a: 1, b: 2, c: 3}
            x.a = x.a * 100
            x.b = x.b * 100
            x.c = x.c * 100
            x
        ";

        let r = get_last_eval(input);

        assert_eq!(
            r,
            Ok(Value::Collection(HashMap::from([
                ("a".to_string(), Value::Int(100)),
                ("b".to_string(), Value::Int(200)),
                ("c".to_string(), Value::Int(300))
            ])))
        )
    }

    #[test]
    fn test_func() {
        let input = "
            x = 1
            y = 2
            add = func(a, b) { return a + b }
            add(x, y)
        ";

        let r = get_last_eval(input);

        assert_eq!(r, Ok(Value::Int(3)));

        let input = "
            x = 1
            y = 2
            add = func(a, b) { return a + b }
            add(x, y) * 10
        ";

        let r = get_last_eval(input);

        assert_eq!(r, Ok(Value::Int(30)));

        let input = "
            x = 1
            y = 2
            add = func(a, b) { return a + b }
            add(x, y) * 10 + 1
        ";

        let r = get_last_eval(input);

        assert_eq!(r, Ok(Value::Int(31)));

        let input = "
            x = 1
            y = 2
            add = func(a, b) { return a + b }
            add(x, y) * 10 + 1
        ";

        let r = get_last_eval(input);

        assert_eq!(r, Ok(Value::Int(31)));

        let input = "
            sequence = [1, 2, 3]
            mutate = func(seq) { seq[0] = 100 }
            mutate(sequence)
            sequence[0]
        ";

        let r = get_last_eval(input);

        assert_eq!(r, Ok(Value::Int(100)));

        let input = "
            collection = { a: 'Hi', b: 'Bye' }
            mutate = func(coll) { coll.a = 'Hello' }
            mutate(collection)
            collection.a
        ";

        let r = get_last_eval(input);

        assert_eq!(r, Ok(Value::StringLiteral("Hello".to_string())));
    }

    #[test]
    fn test_if() {
        let input = "
            x = 1
            y = 2
            r = if x > y {
                x
            } else {
                y
            }
            r
        ";

        let r = get_last_eval(input);

        assert_eq!(r, Ok(Value::Int(2)));

        let input = "
            x = 1
            y = 2
            r = if x < y {
                x
            } else {
                y
            }
            r
        ";

        let r = get_last_eval(input);

        assert_eq!(r, Ok(Value::Int(1)));

        let input = "
            x = 1
            y = 2
            r = if x < y {
                x
            } else if x > y {
                y
            } else {
                0
            }
            r
        ";

        let r = get_last_eval(input);

        assert_eq!(r, Ok(Value::Int(1)));

        let input = "
            x = 1
            y = 2
            r = if x > y {
                x
            } else if x < y {
                y
            } else {
                0
            }
            r
        ";

        let r = get_last_eval(input);

        assert_eq!(r, Ok(Value::Int(2)));

        let input = "
            greet = 'Hello'
            bye = ''

            r = if greet && bye {
                'Hello and Bye'
            } else if greet {
                'Hello'
            } else if bye {
                'Bye'
            } else {
                'Nothing'
            }
            r
        ";

        let r = get_last_eval(input);

        assert_eq!(r, Ok(Value::StringLiteral("Hello".to_string())));
    }

    #[test]
    fn test_references() {
        // `b: a` makes a shallow copy of `a`
        let input = "
            a = [1, 2, 3]
            b = a
            b[0] = 100
            a
        ";

        let r = get_last_eval(input);

        assert_eq!(
            r,
            Ok(Value::Sequence(vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(3)
            ]))
        );

        let input = "
            a = [1, 2, 3]
            b = &a
            b[0] = 100
            a
        ";

        let r = get_last_eval(input);

        assert_eq!(
            r,
            Ok(Value::Sequence(vec![
                Value::Int(100),
                Value::Int(2),
                Value::Int(3)
            ]))
        );

        let input = "
            a = [1, 2, 3]
            b = [&a]
            c = [&b]
            c[0][0][2] = 999
            a
        ";

        let r = get_last_eval(input);

        assert_eq!(
            r,
            Ok(Value::Sequence(vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(999)
            ]))
        );

        let input = "
            a = [1, 2, 3]
            b = [&a]
            b
        ";

        let r = get_last_eval(input);

        assert_eq!(
            r,
            Ok(Value::Sequence(vec![Value::Ref(
                runtime::InnerReference::Identifier("a".to_string())
            )]))
        );

        let input = "
            a = [1, 2, 3]
            b = [&a]
            c = *b
            c
        ";

        let r = get_last_eval(input);

        assert_eq!(
            r,
            Ok(Value::Sequence(vec![Value::Sequence(vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(3)
            ])]))
        );

        let input = "
            d = { inner: 'inner string' }
            c = { d: &d }
            b = { c: &c }
            a = { b: &b }

            a.b.c.d.inner = 'hola a todos'
            d.inner
        ";

        let r = get_last_eval(input);

        assert_eq!(r, Ok(Value::StringLiteral("hola a todos".to_string())));

        // this should do copies of all the objects, so nothing is mutated by ref
        let input = "
            d = { inner: 'inner string' }
            c = { d: d }
            b = { c: c }
            a = { b: b }

            a.b.c.d.inner = 'hola a todos'
            d.inner
        ";

        let r = get_last_eval(input);

        assert_eq!(r, Ok(Value::StringLiteral("inner string".to_string())));
    }

    #[test]
    fn test_collection_methods() {
        let input = "
           collection = {
                keys: [1, 2, 3],
                get_keys: func() {
                    return self.keys
                }
            }

            collection.get_keys()
       ";

        let expected = Value::Sequence(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);

        let r = get_last_eval(input);

        assert_eq!(r, Ok(expected));

        let input = "
            counter = {
                count: 0,
                add: func() {
                    self.count = self.count + 1
                    return self
                }
            }

            counter.add().add().add()
            counter.count
        ";

        let r = get_last_eval(input);

        assert_eq!(r, Ok(Value::Int(3)));

        let input = "
            array = {
                inner: [],
                add: func(n) {
                    self.inner[] = n
                    return self
                }
            }

            array.add(3).add(2).add(1)
            array.inner
        ";

        let expected = Value::Sequence(vec![Value::Int(3), Value::Int(2), Value::Int(1)]);

        let r = get_last_eval(input);

        assert_eq!(r, Ok(expected));

        let input = "
            array = {
                count: 0,
                inner: [],
                add: func() {
                    self.inner[] = self.count
                    self.count = self.count + 1
                    return self
                }
            }

            array.add().add().add()
            array.inner
        ";

        let expected = Value::Sequence(vec![Value::Int(0), Value::Int(1), Value::Int(2)]);

        let r = get_last_eval(input);

        assert_eq!(r, Ok(expected));
    }

    #[test]
    fn test_refs() {
        let input = "
            a = [1, 2, 3]
            b = &a

            b[] = 4
            b[] = 5
            a
        ";

        let expected = Value::Sequence(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
            Value::Int(4),
            Value::Int(5),
        ]);

        let r = get_last_eval(input);
        assert_eq!(r, Ok(expected));

        let input = "
            a = [1, 2, 3]
            b = &a

            b[0] = 10
            b[1] = 20
            b[2] = 30

            a
        ";

        let expected = Value::Sequence(vec![Value::Int(10), Value::Int(20), Value::Int(30)]);

        let r = get_last_eval(input);
        assert_eq!(r, Ok(expected));

        let input = "
            a = { greet: 'Hi', bye: 'Bye' }
            b = &a

            b.greet = 'Hello'
            b.bye = 'GoodBye'

            a
        ";

        let expected = Value::Collection(HashMap::from([
            (
                "greet".to_string(),
                Value::StringLiteral("Hello".to_string()),
            ),
            (
                "bye".to_string(),
                Value::StringLiteral("GoodBye".to_string()),
            ),
        ]));

        let r = get_last_eval(input);
        assert_eq!(r, Ok(expected));
    }

    #[test]
    fn test_for() {
        let input = "
            x = for val in [1, 2, 3] {
                val
            }
            x
        ";

        let expected = Value::Int(3);
        let r = get_last_eval(input);
        assert_eq!(r, Ok(expected));

        let input = "
            counter = 0
            for val in [1, 2, 3] {
                if val % 2 {
                    counter = counter + 1
                }
            }
            counter
        ";

        let expected = Value::Int(2);
        let r = get_last_eval(input);
        assert_eq!(r, Ok(expected));

        let input = "
            counter = 0
            for val in [1, 2, 3] {
                for val in [1, 2, 3] {
                    counter = counter + 1
                }
            }
            counter
        ";

        let expected = Value::Int(9);
        let r = get_last_eval(input);
        assert_eq!(r, Ok(expected));

        let input = "
            counter = 0
            for val in [1, 2, 3] {
                for val in [1, 2, 3] {
                    counter = counter + val
                }
            }
            counter
        ";

        let expected = Value::Int(18);
        let r = get_last_eval(input);
        assert_eq!(r, Ok(expected));

        let input = "
            x = for val in [1, 2, 3, 4, 5, 6] {
                if val % 5 {
                    break 'multiple of 5'
                }
            }
            x
        ";

        let expected = Value::StringLiteral("multiple of 5".to_string());
        let r = get_last_eval(input);
        assert_eq!(r, Ok(expected));

        let input = "
            x = for val in [1, 2, 3, 4, 5, 6] {
                for inner in [1, 2, 3, 4, 5, 6] {
                    if inner + val == 12 {
                        break 'last loop'
                    }
                }
            }
            x
        ";

        let expected = Value::StringLiteral("last loop".to_string());
        let r = get_last_eval(input);
        assert_eq!(r, Ok(expected));

        let input = "
        x = []
        for char in 'Hello' {
            x[] = char
        }
        x
        ";

        let expected = Value::Sequence(vec![
            Value::StringLiteral("H".to_string()),
            Value::StringLiteral("e".to_string()),
            Value::StringLiteral("l".to_string()),
            Value::StringLiteral("l".to_string()),
            Value::StringLiteral("o".to_string()),
        ]);
        let r = get_last_eval(input);
        assert_eq!(r, Ok(expected));

        let input = "
            index = 0
            x = []
            x[] = []
            for char in 'Hello World' {
                if char == ' ' {
                    x[] = []
                    index = index + 1
                    continue
                }
                x[index][] = char
            }
            x
        ";

        let expected = Value::Sequence(vec![
            Value::Sequence(vec![
                Value::StringLiteral("H".to_string()),
                Value::StringLiteral("e".to_string()),
                Value::StringLiteral("l".to_string()),
                Value::StringLiteral("l".to_string()),
                Value::StringLiteral("o".to_string()),
            ]),
            Value::Sequence(vec![
                Value::StringLiteral("W".to_string()),
                Value::StringLiteral("o".to_string()),
                Value::StringLiteral("r".to_string()),
                Value::StringLiteral("l".to_string()),
                Value::StringLiteral("d".to_string()),
            ]),
        ]);

        let r = get_last_eval(input);
        assert_eq!(r, Ok(expected));
    }
}
