#[cfg(test)]
mod scanner_tests {
    use codecrafters_interpreter as lox;

    use lox::scanner::*;
    use lox::token::*;

    fn assert_token_sequence(source: &str, expected: &[(TokenType, &str)]) {
        let scanner = Scanner::new(source.as_bytes());
        let tokens: Vec<_> = scanner.filter_map(Result::ok).collect();

        assert_eq!(tokens.len(), expected.len());

        for (actual, (expected_type, expected_lexeme)) in tokens.iter().zip(expected.iter()) {
            assert_eq!(actual.token_type, *expected_type);
            assert_eq!(actual.lexeme, *expected_lexeme);
        }
    }

    #[test]
    fn test_scanner_01_symbols() {
        assert_token_sequence(
            "({*.,+*})",
            &[
                (TokenType::LEFT_PAREN, "("),
                (TokenType::LEFT_BRACE, "{"),
                (TokenType::STAR, "*"),
                (TokenType::DOT, "."),
                (TokenType::COMMA, ","),
                (TokenType::PLUS, "+"),
                (TokenType::STAR, "*"),
                (TokenType::RIGHT_BRACE, "}"),
                (TokenType::RIGHT_PAREN, ")"),
                (TokenType::EOF, ""),
            ],
        );
    }

    #[test]
    fn test_unexpected_chars_token_sequence() {
        let source = ",.$(#";
        let scanner = Scanner::new(source.as_bytes());

        // Collect all results (both tokens and errors)
        let results: Vec<_> = scanner.collect();

        // Debug output to see actual sequence
        println!("\nActual token/error sequence:");
        for (i, res) in results.iter().enumerate() {
            match res {
                Ok(t) => println!("{}: {:?} '{}'", i, t.token_type, t.lexeme),
                Err(e) => println!("{}: Error: {}", i, e),
            }
        }

        // We expect this sequence:
        // 0: COMMA ','
        // 1: DOT '.'
        // 2: Error for '$'
        // 3: LEFT_PAREN '('
        // 4: Error for '#'
        // 5: EOF

        // Verify we got 6 items (2 valid tokens, 2 errors, 1 valid token, EOF)
        assert_eq!(results.len(), 6, "Expected 6 items in result");

        // Check valid tokens
        assert_token_matches(&results[0], TokenType::COMMA, ",");
        assert_token_matches(&results[1], TokenType::DOT, ".");
        assert_token_matches(&results[3], TokenType::LEFT_PAREN, "(");
        assert_token_matches(&results[5], TokenType::EOF, "");

        // Check errors - we don't assume positions, just that they exist
        let error_count = results.iter().filter(|r| r.is_err()).count();
        assert_eq!(error_count, 2, "Expected 2 error messages");

        for err in results.iter().filter_map(|r| r.as_ref().err()) {
            assert!(
                err.contains("Unexpected token"),
                "Error message should contain 'Unexpected token', got: {}",
                err
            );
        }

        // Helper function
        fn assert_token_matches(
            result: &Result<Token, String>,
            expected_type: TokenType,
            expected_lexeme: &str,
        ) {
            match result {
                Ok(token) => {
                    assert_eq!(
                        token.token_type, expected_type,
                        "Expected token type {:?}, got {:?}",
                        expected_type, token.token_type
                    );
                    assert_eq!(
                        token.lexeme, expected_lexeme,
                        "Expected lexeme '{}', got '{}'",
                        expected_lexeme, token.lexeme
                    );
                }
                Err(e) => panic!("Expected token but got error: {}", e),
            }
        }
    }
}
