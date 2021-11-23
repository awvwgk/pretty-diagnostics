# Pretty diagnostics

This projects provides tools to create diagnostic reports, following the style of the best in class tools in this area, namely ``rustc`` and ``elm``.


## Design

A diagnostic report from ``rustc`` is presented usually in following way (here without color):

```
error[E0000]: main error message
  --> file.rs:LL:CC
   |
LL | <code>
   | -^^^^- secondary label
   |  |
   |  primary label
   |
   = note: note without a `Span`, created with `.note`
note: sub diagnostic message for `.span_note`
  --> file.rs:LL:CC
   |
LL | more code
   |      ^^^^
```

The responsible data type is given by the struct:

```rust
#[must_use]
#[derive(Clone, Debug, PartialEq, Hash, Encodable, Decodable)]
pub struct Diagonstic {
    pub level: Level,
    pub message: Vec<(String, Style)>,
    pub code: Option<DiagnosticId>,
    pub span: MultiSpan,
    pub children: Vec<SubDiagnostic>,
    pub suggestions: Vec<CodeSuggestion>,
}
```

This projects aims to provide and develop the tools to effortlessly construct diagnostic reports in the ``rustc`` style.


## Examples

A common task for presenting all diagnostics is the possiblity to provide a quick possiblility to create a pretty multiline printout:

```f90
use diagnostic, only : render, color_type
implicit none
character(len=*), parameter :: nl = new_line('a')
character(len=*), parameter :: input = &
  "#[must_use]"//nl//&
  "#[derive(Clone, Debug, PartialEq, Hash, Encodable, Decodable)]"//nl//&
  "pub struct Diagonstic {"//nl//&
  "    pub level: Level,"//nl//&
  "    pub message: Vec<(String, Style)>,"//nl//&
  "    pub code: Option<DiagnosticId>,"//nl//&
  "    pub span: MultiSpan,"//nl//&
  "    pub children: Vec<SubDiagnostic>,"//nl//&
  "    pub suggestions: Vec<CodeSuggestion>,"//nl//&
  "}"

print '(a)', render(input, color_type(.true.))
end
```

The above snippet will print the simplified diagnostic struct with line numbers.
The ``render`` function will turn a multiline string input into an output which can be written into a file or the terminal.
The ``color_type`` decides whether the output should be using ANSI escape sequences to make colorful terminal printouts, the logical decides whether stubs are inserted or the actual color printout is produced.

```
   |
 1 | #[must_use]
 2 | #[derive(Clone, Debug, PartialEq, Hash, Encodable, Decodable)]
 3 | pub struct Diagonstic {
 4 |     pub level: Level,
 5 |     pub message: Vec<(String, Style)>,
 6 |     pub code: Option<DiagnosticId>,
 7 |     pub span: MultiSpan,
 8 |     pub children: Vec<SubDiagnostic>,
 9 |     pub suggestions: Vec<CodeSuggestion>,
10 | }
   |
```

The inset on the left side is accounting correctly for the widest line number printout.
For the entire printout no internal input and output is used.

---

To add a diagnostic to the printout a label can be render with the output.
The following snippet will highlight the ``MultiSpan`` member of the struct:

```f90
use diagnostic, only : render, color_type, label_type, level_note
implicit none
character(len=*), parameter :: nl = new_line('a')
character(len=*), parameter :: input = &
  "#[must_use]"//nl//&
  "#[derive(Clone, Debug, PartialEq, Hash, Encodable, Decodable)]"//nl//&
  "pub struct Diagonstic {"//nl//&
  "    pub level: Level,"//nl//&
  "    pub message: Vec<(String, Style)>,"//nl//&
  "    pub code: Option<DiagnosticId>,"//nl//&
  "    pub span: MultiSpan,"//nl//&
  "    pub children: Vec<SubDiagnostic>,"//nl//&
  "    pub suggestions: Vec<CodeSuggestion>,"//nl//&
  "}"

print '(a)', render(input, label_type(level_note, line=7, first=15, last=23), &
  &                 color_type(.true.))
end
```

Running the above snippet should produce a line highlighting ``MultiSpan`` including the surrounding context:

```
  |
6 |     pub code: Option<DiagnosticId>,
7 |     pub span: MultiSpan,
  |               ---------
8 |     pub children: Vec<SubDiagnostic>,
  |
```

---

Some errors

```f90
use diagnostic, only : render, color_type, label_type, level_error
implicit none
character(len=*), parameter :: nl = new_line('a')
character(len=*), parameter :: input = &
  '# This is a TOML document.' // nl // &
  'title = "TOML Example"' // nl // &
  '[owner]' // nl // &
  'name = "Tom Preston-Werner"' // nl // &
  'dob = 1979-05-27T07:32:00-08:00 # First class dates' // nl // &
  '[database]' // nl // &
  'server = "192.168.1.1"' // nl // &
  'ports = [ 8001, 8001, 8002 ]' // nl // &
  'connection_max = 5000' // nl // &
  'enabled = true' // nl // &
  '[servers]' // nl // &
  '  # Indentation (tabs and/or spaces) is allowed but not required' // nl // &
  '  [servers.alpha]' // nl // &
  '  ip = "10.0.0.1"' // nl // &
  '  dc = "eqdc10"' // nl // &
  '  [servers.beta]' // nl // &
  '  ip = "10.0.0.2"' // nl // &
  '  dc = "eqdc10"' // nl // &
  '[title]' // nl // &
  'data = [ ["gamma", "delta"], [1, 2] ]' // nl // &
  '# Line breaks are OK when inside arrays' // nl // &
  'hosts = [' // nl // &
  '  "alpha",' // nl // &
  '  "omega"' // nl // &
  ']'

print '(a)', render(input, &
  &                 [label_type(level_error, line=19, first=2, last=6, primary=.true.), &
  &                  label_type(level_error, line=2, first=1, last=5)], &
  &                 color_type(.true.))
end
```

In this case we expect an output like the one shown here

```
   |
 1 | # This is a TOML document.
 2 | title = "TOML Example"
   | -----
 3 | [owner]
   :
18 |   dc = "eqdc10"
19 | [title]
   |  ^^^^^
20 | data = [ ["gamma", "delta"], [1, 2] ]
   |
```

Note the proper ordering of the annotations and the skipped line between the two highlights.
With this we have the proper mechanism in place to create diagnostic.

---

Now for example with a proper error diagnostic for a duplicated key in a TOML document:

```f90
use diagnostic, only : render, diagnostic_report, color_type, label_type, level_error
implicit none
character(len=*), parameter :: nl = new_line('a')
character(len=*), parameter :: input = &
  '# This is a TOML document.' // nl // &
  'title = "TOML Example"' // nl // &
  '[owner]' // nl // &
  'name = "Tom Preston-Werner"' // nl // &
  'dob = 1979-05-27T07:32:00-08:00 # First class dates' // nl // &
  '[database]' // nl // &
  'server = "192.168.1.1"' // nl // &
  'ports = [ 8001, 8001, 8002 ]' // nl // &
  'connection_max = 5000' // nl // &
  'enabled = true' // nl // &
  '[servers]' // nl // &
  '  # Indentation (tabs and/or spaces) is allowed but not required' // nl // &
  '  [servers.alpha]' // nl // &
  '  ip = "10.0.0.1"' // nl // &
  '  dc = "eqdc10"' // nl // &
  '  [servers.beta]' // nl // &
  '  ip = "10.0.0.2"' // nl // &
  '  dc = "eqdc10"' // nl // &
  '[title]' // nl // &
  'data = [ ["gamma", "delta"], [1, 2] ]' // nl // &
  '# Line breaks are OK when inside arrays' // nl // &
  'hosts = [' // nl // &
  '  "alpha",' // nl // &
  '  "omega"' // nl // &
  ']'

print '(a)', render(diagnostic_report(level_error, &
  & message="duplicated key 'title' found", &
  & source="example.toml", &
  & label=[label_type(level_error, "table 'title' redefined here', 19, 2, 6, .true.), &
  &        label_type(level_error, "first defined here', 2, 1, 5)]), &
  & input, color_type(.true.))
end
```

The resulting error message should look like this:

```
error: duplicated key 'title' found
  --> example.toml:19:2-6
   |
 1 | # This is a TOML document.
 2 | title = "TOML Example"
   | ----- first defined here
 3 | [owner]
   :
18 |   dc = "eqdc10"
19 | [title]
   |  ^^^^^ table 'title' redefined here
20 | data = [ ["gamma", "delta"], [1, 2] ]
   |
```

In a usual parsing situation, the required information would be available from the parse tree, while the constructor feels a bit clunky in an inline example it might work just right when creating the diagnostic.
Also, note that the diagnostic report does not own the input data, but just the labels and messages.
The actual printout is created later by rendering the diagnostic, which also decides whether color output should be used.


## License

This project is free software: you can redistribute it and/or modify it under the terms of the [Apache License, Version 2.0](LICENSE-Apache) or [MIT license](LICENSE-MIT) at your opinion.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an _as is_ basis, without warranties or conditions of any kind, either express or implied. See the License for the specific language governing permissions and limitations under the License.

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in this project by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
