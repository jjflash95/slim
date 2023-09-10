use crate::parser::parse;
use crate::parser::Expr::*;
use crate::parser::Token::*;

mod tests {
    use super::*;

    #[test]
    fn test_primitives() {
        let input = "1";
        assert_eq!(parse(input).unwrap().1, Term(Int(1)));
        let input = "69420";
        assert_eq!(parse(input).unwrap().1, Term(Int(69420)));
        let input = "10e2";
        assert_eq!(parse(input).unwrap().1, Term(Float(1000.0)));
        let input = "10e3";
        assert_eq!(parse(input).unwrap().1, Term(Float(10000.0)));
        let input = "10e-2";
        assert_eq!(parse(input).unwrap().1, Term(Float(0.1)));
        let input = "3.1416";
        assert_eq!(parse(input).unwrap().1, Term(Float(3.1416)));
    }

    #[test]
    fn test_add() {
        let input = "x = 1 + 2";
        let result = parse(input);
        let expected = Assign {
            target: Term(Identifier("x".to_owned())).into(),
            value: Binary {
                op: Add,
                left: Term(Int(1)).into(),
                right: Term(Int(2)).into(),
            }
            .into(),
        };
        assert_eq!(result.unwrap().1, expected)
    }

    #[test]
    fn test_combined() {
        let input = "x  = 1 * 4 + 2";
        let result = parse(input);
        let expected = Assign {
            target: Term(Identifier("x".to_owned())).into(),
            value: Binary {
                op: Add,
                left: Binary {
                    op: Multiply,
                    left: Term(Int(1)).into(),
                    right: Term(Int(4)).into(),
                }
                .into(),
                right: Term(Int(2)).into(),
            }
            .into(),
        };

        assert_eq!(result.unwrap().1, expected);

        let input = "x  = 1 * 4 + 2";
        let result = parse(input).unwrap().1;
        let input = "x  = (1 * 4) + 2";
        assert_eq!(result, parse(input).unwrap().1);

        let input = "x  = 1 / 4 * 4 - 7 * 2";
        let result = parse(input).unwrap().1;
        let input = "x  = ((1 / 4) * 4) - (7 * 2)";
        assert_eq!(result, parse(input).unwrap().1);
    }

    #[test]
    fn test_compare() {
        let input = "x  = 2 + 1 > 1 * 3";
        let expected = Assign {
            target: Term(Identifier("x".to_owned())).into(),
            value: Binary {
                op: Gt,
                left: Binary {
                    op: Add,
                    left: Term(Int(2)).into(),
                    right: Term(Int(1)).into(),
                }
                .into(),
                right: Binary {
                    op: Multiply,
                    left: Term(Int(1)).into(),
                    right: Term(Int(3)).into(),
                }
                .into(),
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "x  = true == false";
        let expected = Assign {
            target: Term(Identifier("x".to_owned())).into(),
            value: Binary {
                op: Eq,
                left: Term(True).into(),
                right: Term(False).into(),
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "x  = true != false";
        let expected = Assign {
            target: Term(Identifier("x".to_owned())).into(),
            value: Binary {
                op: Ne,
                left: Term(True).into(),
                right: Term(False).into(),
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);
    }

    #[test]
    fn parse_andor() {
        let input = "x  = true && 2 + 1";
        let expected = Assign {
            target: Term(Identifier("x".to_owned())).into(),
            value: Binary {
                op: And,
                left: Term(True).into(),
                right: Binary {
                    op: Add,
                    left: Term(Int(2)).into(),
                    right: Term(Int(1)).into(),
                }
                .into(),
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "x  = true && true || false";
        let expected = Assign {
            target: Term(Identifier("x".to_owned())).into(),
            value: Binary {
                op: Or,
                left: Binary {
                    op: And,
                    left: Term(True).into(),
                    right: Term(True).into(),
                }
                .into(),
                right: Term(False).into(),
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);
    }

    #[test]
    pub fn test_collection() {
        let input = "x  = { a: 1, b: { lol: 2 }}";
        let expected = Assign {
            target: Term(Identifier("x".to_owned())).into(),
            value: Collection(vec![
                Assign {
                    target: Term(Identifier("a".to_owned())).into(),
                    value: Term(Int(1)).into(),
                },
                Assign {
                    target: Term(Identifier("b".to_owned())).into(),
                    value: Collection(vec![Assign {
                        target: Term(Identifier("lol".to_owned())).into(),
                        value: Term(Int(2)).into(),
                    }])
                    .into(),
                },
            ])
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);
    }

    #[test]
    pub fn test_sequence() {
        let input = "x  = [1, 2, 3]";
        let expected = Assign {
            target: Term(Identifier("x".to_owned())).into(),
            value: Sequence(vec![Term(Int(1)), Term(Int(2)), Term(Int(3))]).into(),
        };
        assert_eq!(parse(input).unwrap().1, expected);

        let input = "[1, 2, 3, [4, 5, {a: 6}]]";
        let expected = Sequence(vec![
            Term(Int(1)),
            Term(Int(2)),
            Term(Int(3)),
            Sequence(vec![
                Term(Int(4)),
                Term(Int(5)),
                Collection(vec![Assign {
                    target: Term(Identifier("a".to_owned())).into(),
                    value: Term(Int(6)).into(),
                }]),
            ]),
        ]);
        assert_eq!(parse(input).unwrap().1, expected);
    }

    #[test]
    fn test_call() {
        let input = "x = a(1)";
        let expected = Assign {
            target: Term(Identifier("x".to_owned())).into(),
            value: Call {
                target: Term(Identifier("a".to_owned())).into(),
                args: vec![Term(Int(1))],
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "x  = a(1)(2)";
        let expected = Assign {
            target: Term(Identifier("x".to_owned())).into(),
            value: Call {
                target: Call {
                    target: Term(Identifier("a".to_owned())).into(),
                    args: vec![Term(Int(1))],
                }
                .into(),
                args: vec![Term(Int(2))],
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "call_func(true && false)";
        let expected = Call {
            target: Term(Identifier("call_func".to_owned())).into(),
            args: vec![Binary {
                op: And,
                left: Term(True).into(),
                right: Term(False).into(),
            }],
        };

        assert_eq!(parse(input).unwrap().1, expected);
    }

    #[test]
    fn test_parse_access() {
        let input = "obj.a";
        let expected = Access {
            target: Term(Identifier("obj".to_owned())).into(),
            field: Term(StringLiteral("a".to_owned())).into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "obj.a.b";
        let expected = Access {
            target: Access {
                target: Term(Identifier("obj".to_owned())).into(),
                field: Term(StringLiteral("a".to_owned())).into(),
            }
            .into(),
            field: Term(StringLiteral("b".to_owned())).into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "sequence[0][1]";
        let expected = Access {
            target: Access {
                target: Term(Identifier("sequence".to_owned())).into(),
                field: Term(Int(0)).into(),
            }
            .into(),
            field: Term(Int(1)).into(),
        };
        assert_eq!(parse(input).unwrap().1, expected);

        let input = "sequence[0]()[1]()";
        let expected = Call {
            target: Access {
                target: Call {
                    target: Access {
                        target: Term(Identifier("sequence".to_owned())).into(),
                        field: Term(Int(0)).into(),
                    }
                    .into(),
                    args: vec![],
                }
                .into(),
                field: Term(Int(1)).into(),
            }
            .into(),
            args: vec![],
        };

        assert_eq!(parse(input).unwrap().1, expected);
    }

    #[test]
    fn test_access_call() {
        let input = "obj.a()";
        let expected = Call {
            target: Access {
                target: Term(Identifier("obj".to_owned())).into(),
                field: Term(StringLiteral("a".to_owned())).into(),
            }
            .into(),
            args: vec![],
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "obj.a().b()";
        let expected = Call {
            target: Access {
                target: Call {
                    target: Access {
                        target: Term(Identifier("obj".to_owned())).into(),
                        field: Term(StringLiteral("a".to_owned())).into(),
                    }
                    .into(),
                    args: vec![],
                }
                .into(),
                field: Term(StringLiteral("b".to_owned())).into(),
            }
            .into(),
            args: vec![],
        };

        assert_eq!(parse(input).unwrap().1, expected);
    }

    #[test]
    fn test_assign_collection() {
        let input = "x.a.b  = 10";
        let expected = Assign {
            target: Access {
                target: Access {
                    target: Term(Identifier("x".to_owned())).into(),
                    field: Term(StringLiteral("a".to_owned())).into(),
                }
                .into(),
                field: Term(StringLiteral("b".to_owned())).into(),
            }
            .into(),
            value: Term(Int(10)).into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "s[0][1]  = 20";
        let expected = Assign {
            target: Access {
                target: Access {
                    target: Term(Identifier("s".to_owned())).into(),
                    field: Term(Int(0)).into(),
                }
                .into(),
                field: Term(Int(1)).into(),
            }
            .into(),
            value: Term(Int(20)).into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);
    }

    #[test]
    fn test_func_decl() {
        let input = "func() {}";
        let expected = Func {
            name: None,
            params: vec![],
            body: vec![],
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "func() { x  = 1 }";
        let expected = Func {
            name: None,
            params: vec![],
            body: vec![Assign {
                target: Term(Identifier("x".to_owned())).into(),
                value: Term(Int(1)).into(),
            }],
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "func() {
            x  = 1
            y  = 2
        }";
        let expected = Func {
            name: None,
            params: vec![],
            body: vec![
                Assign {
                    target: Term(Identifier("x".to_owned())).into(),
                    value: Term(Int(1)).into(),
                },
                Assign {
                    target: Term(Identifier("y".to_owned())).into(),
                    value: Term(Int(2)).into(),
                },
            ],
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "func() {
            x  = 1
            y  = 2
            z  = 3
        }";
        let expected = Func {
            name: None,
            params: vec![],
            body: vec![
                Assign {
                    target: Term(Identifier("x".to_owned())).into(),
                    value: Term(Int(1)).into(),
                },
                Assign {
                    target: Term(Identifier("y".to_owned())).into(),
                    value: Term(Int(2)).into(),
                },
                Assign {
                    target: Term(Identifier("z".to_owned())).into(),
                    value: Term(Int(3)).into(),
                },
            ],
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "func sum(a,b) {
            return a + b
        }";

        let expected = Func {
            name: Some("sum".to_owned()),
            params: vec![
                Term(Identifier("a".to_owned())),
                Term(Identifier("b".to_owned())),
            ],
            body: vec![Return(
                Binary {
                    op: Add,
                    left: Term(Identifier("a".to_owned())).into(),
                    right: Term(Identifier("b".to_owned())).into(),
                }
                .into(),
            )],
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "x = func sum(a, b) {
            return a + b
        }";

        let expected = Assign {
            target: Term(Identifier("x".to_owned())).into(),
            value: Func {
                name: Some("sum".to_owned()),
                params: vec![
                    Term(Identifier("a".to_owned())),
                    Term(Identifier("b".to_owned())),
                ],
                body: vec![Return(
                    Binary {
                        op: Add,
                        left: Term(Identifier("a".to_owned())).into(),
                        right: Term(Identifier("b".to_owned())).into(),
                    }
                    .into(),
                )],
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "func sum(a, b) {
            return a + b
        }(1, 2)";

        let expected = Call {
            target: Func {
                name: Some("sum".to_owned()),
                params: vec![
                    Term(Identifier("a".to_owned())),
                    Term(Identifier("b".to_owned())),
                ],
                body: vec![Return(
                    Binary {
                        op: Add,
                        left: Term(Identifier("a".to_owned())).into(),
                        right: Term(Identifier("b".to_owned())).into(),
                    }
                    .into(),
                )],
            }
            .into(),
            args: vec![Term(Int(1)), Term(Int(2))],
        };

        let res = parse(input).unwrap();
        assert_eq!(res.1, expected);

        let input = "sum_res = func (a, b) {
            return a + b
        }(420 + 69)
        ";

        let expected = Assign {
            target: Term(Identifier("sum_res".to_owned())).into(),
            value: Call {
                target: Func {
                    name: None,
                    params: vec![
                        Term(Identifier("a".to_owned())),
                        Term(Identifier("b".to_owned())),
                    ],
                    body: vec![Return(
                        Binary {
                            op: Add,
                            left: Term(Identifier("a".to_owned())).into(),
                            right: Term(Identifier("b".to_owned())).into(),
                        }
                        .into(),
                    )],
                }
                .into(),
                args: vec![Binary {
                    op: Add,
                    left: Term(Int(420)).into(),
                    right: Term(Int(69)).into(),
                }],
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "s[0][1](params)";
        let expected = Call {
            target: Access {
                target: Access {
                    target: Term(Identifier("s".to_owned())).into(),
                    field: Term(Int(0)).into(),
                }
                .into(),
                field: Term(Int(1)).into(),
            }
            .into(),
            args: vec![Term(Identifier("params".to_owned()))],
        };
        assert_eq!(parse(input).unwrap().1, expected);
    }

    #[test]
    fn test_for_loop() {
        let input = "y = for value in sequence {}";
        let expected = Assign {
            target: Term(Identifier("y".to_owned())).into(),
            value: For {
                pin: Term(Identifier("value".to_owned())).into(),
                iterable: Term(Identifier("sequence".to_owned())).into(),
                body: vec![],
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "y = for value in [1,2,3] {
            x  = x + value
        }";
        let expected = Assign {
            target: Term(Identifier("y".to_owned())).into(),
            value: For {
                pin: Term(Identifier("value".to_owned())).into(),
                iterable: Sequence(vec![Term(Int(1)), Term(Int(2)), Term(Int(3))]).into(),
                body: vec![Assign {
                    target: Term(Identifier("x".to_owned())).into(),
                    value: Binary {
                        op: Add,
                        left: Term(Identifier("x".to_owned())).into(),
                        right: Term(Identifier("value".to_owned())).into(),
                    }
                    .into(),
                }],
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);
    }

    #[test]
    fn test_while_loop() {
        let input = "while false {}";
        let expected = While {
            pin: Term(False).into(),
            body: vec![],
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "y = while true {}";
        let expected = Assign {
            target: Term(Identifier("y".to_owned())).into(),
            value: While {
                pin: Term(True).into(),
                body: vec![],
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "y = while true {
            x = x + 1
        }";
        let expected = Assign {
            target: Term(Identifier("y".to_owned())).into(),
            value: While {
                pin: Term(True).into(),
                body: vec![Assign {
                    target: Term(Identifier("x".to_owned())).into(),
                    value: Binary {
                        op: Add,
                        left: Term(Identifier("x".to_owned())).into(),
                        right: Term(Int(1)).into(),
                    }
                    .into(),
                }],
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "y = while z < 10 {
            z = z + 1
        }";
        let expected = Assign {
            target: Term(Identifier("y".to_owned())).into(),
            value: While {
                pin: Binary {
                    op: Lt,
                    left: Term(Identifier("z".to_owned())).into(),
                    right: Term(Int(10)).into(),
                }
                .into(),
                body: vec![Assign {
                    target: Term(Identifier("z".to_owned())).into(),
                    value: Binary {
                        op: Add,
                        left: Term(Identifier("z".to_owned())).into(),
                        right: Term(Int(1)).into(),
                    }
                    .into(),
                }],
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "y = while z < 10 {
            z  = z + 1
            if z > 5 {
                break z
            }
        }";

        let expected = Assign {
            target: Term(Identifier("y".to_owned())).into(),
            value: While {
                pin: Binary {
                    op: Lt,
                    left: Term(Identifier("z".to_owned())).into(),
                    right: Term(Int(10)).into(),
                }
                .into(),
                body: vec![
                    Assign {
                        target: Term(Identifier("z".to_owned())).into(),
                        value: Binary {
                            op: Add,
                            left: Term(Identifier("z".to_owned())).into(),
                            right: Term(Int(1)).into(),
                        }
                        .into(),
                    },
                    Conditional {
                        branches: vec![(
                            Binary {
                                op: Gt,
                                left: Term(Identifier("z".to_owned())).into(),
                                right: Term(Int(5)).into(),
                            },
                            vec![Break(Term(Identifier("z".to_owned())).into())],
                        )],
                    },
                ],
            }
            .into(),
        };
        assert_eq!(parse(input).unwrap().1, expected);
    }

    #[test]
    fn test_loop() {
        let input = "x = loop { break 10 }";
        let expected = Assign {
            target: Term(Identifier("x".to_owned())).into(),
            value: Loop {
                body: vec![Break(Term(Int(10)).into())],
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "x = loop {
            counter  = counter + 1
            if counter > 10 {
                break counter
            }
        }";

        let expected = Assign {
            target: Term(Identifier("x".to_owned())).into(),
            value: Loop {
                body: vec![
                    Assign {
                        target: Term(Identifier("counter".to_owned())).into(),
                        value: Binary {
                            op: Add,
                            left: Term(Identifier("counter".to_owned())).into(),
                            right: Term(Int(1)).into(),
                        }
                        .into(),
                    },
                    Conditional {
                        branches: vec![(
                            Binary {
                                op: Gt,
                                left: Term(Identifier("counter".to_owned())).into(),
                                right: Term(Int(10)).into(),
                            },
                            vec![Break(Term(Identifier("counter".to_owned())).into())],
                        )],
                    },
                ],
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);
    }

    #[test]
    fn test_unary() {
        let input = "x = !true";
        let expected = Assign {
            target: Term(Identifier("x".to_owned())).into(),
            value: Unary {
                op: Not,
                value: Term(True).into(),
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "x = -one";
        let expected = Assign {
            target: Term(Identifier("x".to_owned())).into(),
            value: Unary {
                op: Negative,
                value: Term(Identifier("one".to_owned())).into(),
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);
    }

    #[test]
    fn test_pipes() {
        let input = "1 & double & sub_one";
        let expected = Pipe {
            parent: Term(Identifier("sub_one".to_owned())).into(),
            child: Pipe {
                parent: Term(Identifier("double".to_owned())).into(),
                child: Term(Int(1)).into(),
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "1 & double & sub(1)";
        let expected = Pipe {
            parent: Call {
                target: Term(Identifier("sub".to_owned())).into(),
                args: vec![Term(Int(1))],
            }
            .into(),
            child: Pipe {
                parent: Term(Identifier("double".to_owned())).into(),
                child: Term(Int(1)).into(),
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "trimmed_uppercase_str = ' this is my string\t' & trim & uppercase";
        let expected = Assign {
            target: Term(Identifier("trimmed_uppercase_str".to_owned())).into(),
            value: Pipe {
                parent: Term(Identifier("uppercase".to_owned())).into(),
                child: Pipe {
                    parent: Term(Identifier("trim".to_owned())).into(),
                    child: Term(StringLiteral(" this is my string\t".to_owned())).into(),
                }
                .into(),
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "hello_world = 'Hello' & push(' world') & push('!')";
        let expected = Assign {
            target: Term(Identifier("hello_world".to_owned())).into(),
            value: Pipe {
                parent: Call {
                    target: Term(Identifier("push".to_owned())).into(),
                    args: vec![Term(StringLiteral("!".to_owned()))],
                }
                .into(),
                child: Pipe {
                    parent: Call {
                        target: Term(Identifier("push".to_owned())).into(),
                        args: vec![Term(StringLiteral(" world".to_owned()))],
                    }
                    .into(),
                    child: Term(StringLiteral("Hello".to_owned())).into(),
                }
                .into(),
            }
            .into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);
    }

    #[test]
    fn test_collection_with_funcs() {
        let input = "{
            x: 'string',
            y: 10e2,
            z: func() {
                return 0
            }
        }
        ";

        let expected = Collection(vec![
            Assign {
                target: Term(Identifier("x".to_owned())).into(),
                value: Term(StringLiteral("string".to_owned())).into(),
            },
            Assign {
                target: Term(Identifier("y".to_owned())).into(),
                value: Term(Float(1000.0)).into(),
            },
            Assign {
                target: Term(Identifier("z".to_owned())).into(),
                value: Func {
                    name: None,
                    params: vec![],
                    body: vec![Return(Term(Int(0)).into())],
                }
                .into(),
            },
        ]);

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "{
            x: 'string',
            y: 10e2,
            z: func() {
                return self.x
            }
        }
        ";

        let expected = Collection(vec![
            Assign {
                target: Term(Identifier("x".to_owned())).into(),
                value: Term(StringLiteral("string".to_owned())).into(),
            },
            Assign {
                target: Term(Identifier("y".to_owned())).into(),
                value: Term(Float(1000.0)).into(),
            },
            Assign {
                target: Term(Identifier("z".to_owned())).into(),
                value: Func {
                    name: None,
                    params: vec![],
                    body: vec![Return(
                        Access {
                            target: Term(_Self).into(),
                            field: Term(StringLiteral("x".to_owned())).into(),
                        }
                        .into(),
                    )],
                }
                .into(),
            },
        ]);

        assert_eq!(parse(input).unwrap().1, expected);
    }

    #[test]
    fn test_derefs() {
        let input = "b = *a";
        let expected = Assign {
            target: Term(Identifier("b".to_owned())).into(),
            value: Deref(Term(Identifier("a".to_owned())).into()).into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);

        let input = "b = **a";
        let expected = Assign {
            target: Term(Identifier("b".to_owned())).into(),
            value: Deref(Deref(Term(Identifier("a".to_owned())).into()).into()).into(),
        };

        assert_eq!(parse(input).unwrap().1, expected);
    }
}
