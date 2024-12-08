// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

use babbelaar::{BabString, FileRange, SemanticDiagnosticSeverity, SourceCode};
use colored::{Color, ColoredString, Colorize};

pub struct ErrorPrinter {
    severity: SemanticDiagnosticSeverity,
    source_code: SourceCode,
    range: FileRange,
    message: String,
    hint: Option<String>,

    color: Color,
    line_number: ColoredString,
}

impl ErrorPrinter {
    #[must_use = "Use the `print` method to actually print"]
    pub fn new(source_code: &SourceCode, range: FileRange, message: impl Display) -> Self {
        Self {
            source_code: source_code.clone(),
            range,
            severity: SemanticDiagnosticSeverity::Error,
            message: message.to_string(),
            hint: None,

            color: Color::Red,
            line_number: format!("{}", range.start().line() + 1).to_string().blue().bold(),
        }
    }

    #[must_use]
    pub fn hint(self, hint: impl Into<Option<String>>) -> Self {
        Self {
            hint: hint.into().map(|x| x.to_string()),
            ..self
        }
    }

    #[must_use]
    pub fn severity(self, severity: SemanticDiagnosticSeverity) -> Self {
        Self {
            severity,
            color: match severity {
                SemanticDiagnosticSeverity::Error => Color::Red,
                SemanticDiagnosticSeverity::Warning => Color::Yellow,
            },
            ..self
        }
    }

    pub fn print(self) {
        self.print_prelude();

        self.print_lines();

        self.print_postlude();
    }

    fn print_prelude(&self) {
        let severity_string = match self.severity {
            SemanticDiagnosticSeverity::Error => "fout".red().bold(),
            SemanticDiagnosticSeverity::Warning => "waarschuwing".yellow().bold(),
        };
        eprintln!("{severity_string}: {}", self.message.to_string().bold());

        eprintln!();
    }

    fn print_lines(&self) {
        let lines: Vec<&str> = self.source_code.lines().skip(self.range.start().line().saturating_sub(1)).take(3).collect();

        if !lines[0].trim().is_empty() {
            self.print_line(lines.len() == 1, lines[0]);
        }

        if let Some(line) = lines.get(1) {
            self.print_line(true, line);
        }

        self.print_error_indicator();

        if let Some(line) = lines.get(2) {
            self.print_line(false, line);
        }
    }

    fn print_line(&self, is_primary: bool, line: &str) {
        self.print_line_prefix(is_primary);
        eprintln!("{line}");
    }

    fn print_error_indicator(&self) {
        let spaces = " ".repeat(self.range.start().column());
        let caret = "^".color(self.color).bold();
        let tildes = "~".repeat(self.range.len().saturating_sub(1)).color(self.color);

        let hint = match &self.hint {
            Some(hint) => format!("hint: {hint}").color(self.color).bold(),
            None => "".bold(),
        };

        self.print_line_prefix(false);
        eprintln!("{spaces}{caret}{tildes} {hint}");
    }

    fn print_line_prefix(&self, is_primary: bool) {
        let separator = " | ".blue().bold();

        if is_primary {
            eprint!("{} {separator}", self.line_number);
        } else {
            eprint!("{} {separator}", " ".repeat(self.line_number.len()));
        }
    }

    fn print_postlude(&self) {
        eprintln!();

        let path = self.source_code.path().display();
        let line = self.range.start().line() + 1;
        let column = self.range.start().column() + 1;

        eprintln!("In {path}:{line}:{column}\n");
    }
}

#[derive(Debug, Clone)]
pub struct RuntimeError {
    message: BabString,
}

impl RuntimeError {
    #[must_use]
    pub fn array_out_of_bounds(array_size: usize, index: i64) -> Self {
        Self {
            message: format!("Opeenvolging-index is buiten bereik, index={index} terwijl grootte={array_size}").into(),
        }
    }

    #[must_use]
    pub fn message(&self) -> BabString {
        self.message.clone()
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}
