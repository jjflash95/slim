mod tests {

    use std::collections::HashMap;

    use crate::parser::{parse, ParseResult};
    use crate::runtime::object::ToObject;
    use crate::runtime::scope::Scope;
    use crate::runtime::Object;
    use crate::runtime::{self};

    fn get_last_eval(mut input: &str) -> Object {
        let mut ctx = Scope::default();
        let mut r = Object::Nil;
        loop {
            match parse(input) {
                Ok((i, ast)) => {
                    r = match ast {
                        ParseResult::Expression(ast) => {
                            runtime::evaluate(&mut ctx, ast).unwrap().object()
                        }
                        ParseResult::Statement(ast) => {
                            runtime::evaluate_stmt(&mut ctx, ast);
                            Object::Nil
                        }
                    };
                    input = i;
                }
                Err(_) => {
                    break;
                }
            };
        }

        r
    }

    // #[test]
    // fn test_operations() {
    //     let input = "1 + 2";
    //     let mut ctx = Scope::default();
    //     let ast = parse(input).unwrap().1;
    //     let r = runtime::evaluate(&mut ctx, ast).unwrap().object();
    //     assert_eq!(r, Object::Int(3));

    //     let input = "1.0 + 2.5";
    //     let mut ctx = Scope::default();
    //     let ast = parse(input).unwrap().1;
    //     let r = runtime::evaluate(&mut ctx, ast).unwrap().object();
    //     assert_eq!(r, Object::Float(3.5));

    //     let input = "10.0 + 3 * 10";
    //     let mut ctx = Scope::default();
    //     let ast = parse(input).unwrap().1;
    //     let r = runtime::evaluate(&mut ctx, ast).unwrap().object();
    //     assert_eq!(r, Object::Float(40.0));

    //     let input = "10.0 + 3 * 10 + 2";
    //     let mut ctx = Scope::default();
    //     let ast = parse(input).unwrap().1;
    //     let r = runtime::evaluate(&mut ctx, ast).unwrap().object();
    //     assert_eq!(r, Object::Float(42.0));

    //     let input = "66 / 3";
    //     let mut ctx = Scope::default();
    //     let ast = parse(input).unwrap().1;
    //     let r = runtime::evaluate(&mut ctx, ast).unwrap().object();
    //     assert_eq!(r, Object::Float(22.0));

    //     let input = "66 / -3";
    //     let mut ctx = Scope::default();
    //     let ast = parse(input).unwrap().1;
    //     let r = runtime::evaluate(&mut ctx, ast).unwrap().object();
    //     assert_eq!(r, Object::Float(-22.0));
    // }

    // #[test]
    // fn test_sequences() {
    //     let input = "[1, 2, 3]";
    //     let mut ctx = Scope::default();
    //     let ast = parse(input).unwrap().1;
    //     let r = runtime::evaluate(&mut ctx, ast).unwrap().object();
    //     assert_eq!(
    //         r,
    //         Object::Sequence(vec![
    //             Object::Int(1).into(),
    //             Object::Int(2).into(),
    //             Object::Int(3).into()
    //         ])
    //     );

    //     let input = "
    //         x = [1, 2, 3]
    //         x[0] = 100
    //         x
    //     ";

    //     let r = get_last_eval(input);

    //     assert_eq!(
    //         r,
    //         Object::Sequence(vec![
    //             Object::Int(100).into(),
    //             Object::Int(2).into(),
    //             Object::Int(3).into()
    //         ])
    //     );

    //     let input = "
    //         x = [1, 2, 3]
    //         x[0] = x[0] * 100
    //         x[1] = x[1] * 100
    //         x[2] = x[2] * 100
    //         x
    //     ";

    //     let r = get_last_eval(input);

    //     assert_eq!(
    //         r,
    //         Object::Sequence(vec![
    //             Object::Int(100).into(),
    //             Object::Int(200).into(),
    //             Object::Int(300).into()
    //         ])
    //     );

    //     let input = "
    //         x = [1, 2, 3]
    //         index = 1
    //         x[index] = 999
    //         x
    //     ";

    //     let r = get_last_eval(input);

    //     assert_eq!(
    //         r,
    //         Object::Sequence(vec![
    //             Object::Int(1).into(),
    //             Object::Int(999).into(),
    //             Object::Int(3).into()
    //         ])
    //     );
    // }

    // #[test]
    // fn test_collection() {
    //     let input = "{a: 1, b: 2, c: 3}";
    //     let mut ctx = Scope::default();
    //     let ast = parse(input).unwrap().1;
    //     let r = runtime::evaluate(&mut ctx, ast).unwrap().object();
    //     assert_eq!(
    //         r,
    //         Object::Collection(HashMap::from([
    //             ("a".to_string(), Object::Int(1).into()),
    //             ("b".to_string(), Object::Int(2).into()),
    //             ("c".to_string(), Object::Int(3).into())
    //         ]))
    //     );

    //     let input = "
    //         x = {a: 1, b: 2, c: 3}
    //         x.a = 100
    //         x
    //     ";

    //     let r = get_last_eval(input);

    //     assert_eq!(
    //         r,
    //         Object::Collection(HashMap::from([
    //             ("a".to_string(), Object::Int(100).into()),
    //             ("b".to_string(), Object::Int(2).into()),
    //             ("c".to_string(), Object::Int(3).into())
    //         ]))
    //     );

    //     let input = "
    //         x = {a: 1, b: 2, c: 3}
    //         x.a = x.a * 100
    //         x.b = x.b * 100
    //         x.c = x.c * 100
    //         x
    //     ";

    //     let r = get_last_eval(input);

    //     assert_eq!(
    //         r,
    //         Object::Collection(HashMap::from([
    //             ("a".to_string(), Object::Int(100).into()),
    //             ("b".to_string(), Object::Int(200).into()),
    //             ("c".to_string(), Object::Int(300).into())
    //         ]))
    //     )
    // }

    #[test]
    fn test_func() {
        let input = "
            x = 1
            y = 2
            add = func(a, b) { return a + b }
            add(x, y)
        ";

        let r = get_last_eval(input);

        assert_eq!(r, Object::Int(3));

        let input = "
            x = 1
            y = 2
            add = func(a, b) { return a + b }
            add(x, y) * 10
        ";

        let r = get_last_eval(input);

        assert_eq!(r, Object::Int(30));

        let input = "
            x = 1
            y = 2
            add = func(a, b) { return a + b }
            add(x, y) * 10 + 1
        ";

        let r = get_last_eval(input);

        assert_eq!(r, Object::Int(31));

        let input = "
            x = 1
            y = 2
            add = func(a, b) { return a + b }
            add(x, y) * 10 + 1
        ";

        let r = get_last_eval(input);

        assert_eq!(r, Object::Int(31));

        let input = "
            sequence = [1, 2, 3]
            mutate = func(s) { s[0] = 100 }
            mutate(sequence)
            sequence[0]
        ";

        let r = get_last_eval(input);

        assert_eq!(r, Object::Int(100));

        let input = "
            collection = { a: 'Hi', b: 'Bye' }
            mutate = func(coll) { coll.a = 'Hello' }
            mutate(collection)
            collection.a
        ";

        let r = get_last_eval(input);

        assert_eq!(r, Object::Str("Hello".to_string()));
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

        assert_eq!(r, Object::Int(2));

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

        assert_eq!(r, Object::Int(1));

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

        assert_eq!(r, Object::Int(1));

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

        assert_eq!(r, Object::Int(2));

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

        assert_eq!(r, Object::Str("Hello".to_string()));
    }

    #[test]
    fn test_references() {
        let input = "
            d = { inner: 'inner string' }
            c = { d: d }
            b = { c: c }
            a = { b: b }

            a.b.c.d.inner = 'hola a todos'
            d.inner
        ";

        let r = get_last_eval(input);
        assert_eq!(r, Object::Str("hola a todos".to_string()));
    }

    #[test]
    fn test_refs() {
        let input = "
            a = [1, 2, 3]
            b = a

            b[] = 4
            b[] = 5
            a
        ";

        let expected = Object::Sequence(vec![
            Object::Int(1).into(),
            Object::Int(2).into(),
            Object::Int(3).into(),
            Object::Int(4).into(),
            Object::Int(5).into(),
        ]);

        let r = get_last_eval(input);
        assert_eq!(r, expected);

        let input = "
            a = [1, 2, 3]
            b = a

            b[0] = 10
            b[1] = 20
            b[2] = 30

            a
        ";

        let expected = Object::Sequence(vec![
            Object::Int(10).into(),
            Object::Int(20).into(),
            Object::Int(30).into(),
        ]);

        let r = get_last_eval(input);
        assert_eq!(r, expected);

        let input = "
            a = { greet: 'Hi', bye: 'Bye' }
            b = a

            b.greet = 'Hello'
            b.bye = 'GoodBye'

            a
        ";

        let expected = Object::Collection(HashMap::from([
            ("greet".to_string(), Object::Str("Hello".to_string()).into()),
            ("bye".to_string(), Object::Str("GoodBye".to_string()).into()),
        ]));

        let r = get_last_eval(input);
        assert_eq!(r, expected);
    }

    #[test]
    fn test_for() {
        let input = "
            x = for val in [1, 2, 3] {
                val
            }
            x
        ";

        let expected = Object::Int(3);
        let r = get_last_eval(input);
        assert_eq!(r, expected);

        let input = "
            counter = 0
            for val in [1, 2, 3] {
                if val % 2 {
                    counter = counter + 1
                }
            }
            counter
        ";

        let expected = Object::Int(2);
        let r = get_last_eval(input);
        assert_eq!(r, expected);

        let input = "
            counter = 0
            for val in [1, 2, 3] {
                for val in [1, 2, 3] {
                    counter = counter + 1
                }
            }
            counter
        ";

        let expected = Object::Int(9);
        let r = get_last_eval(input);
        assert_eq!(r, expected);

        let input = "
            counter = 0
            for val in [1, 2, 3] {
                for val in [1, 2, 3] {
                    counter = counter + val
                }
            }
            counter
        ";

        let expected = Object::Int(18);
        let r = get_last_eval(input);
        assert_eq!(r, expected);

        let input = "
            x = for val in [1, 2, 3, 4, 5, 6] {
                if val % 5 {
                    break 'multiple of 5'
                }
            }
            x
        ";

        let expected = Object::Str("multiple of 5".to_string());
        let r = get_last_eval(input);
        assert_eq!(r, expected);

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

        let expected = Object::Str("last loop".to_string());
        let r = get_last_eval(input);
        assert_eq!(r, expected);

        let input = "
        x = []
        for char in 'Hello' {
            x[] = char
        }
        x
        ";

        let expected = Object::Sequence(vec![
            Object::Str("H".to_string()).into(),
            Object::Str("e".to_string()).into(),
            Object::Str("l".to_string()).into(),
            Object::Str("l".to_string()).into(),
            Object::Str("o".to_string()).into(),
        ]);
        let r = get_last_eval(input);
        assert_eq!(r, expected);

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

        let expected = Object::Sequence(vec![
            Object::Sequence(vec![
                Object::Str("H".to_string()).into(),
                Object::Str("e".to_string()).into(),
                Object::Str("l".to_string()).into(),
                Object::Str("l".to_string()).into(),
                Object::Str("o".to_string()).into(),
            ])
            .into(),
            Object::Sequence(vec![
                Object::Str("W".to_string()).into(),
                Object::Str("o".to_string()).into(),
                Object::Str("r".to_string()).into(),
                Object::Str("l".to_string()).into(),
                Object::Str("d".to_string()).into(),
            ])
            .into(),
        ]);

        let r = get_last_eval(input);
        assert_eq!(r, expected);
    }
}
