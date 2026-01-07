# Testing

- Test must strive for single-line asserts
- Try to avoid multi-line assertings
- When using "asserting", try to make it in form of: .asserting(_ ...)
- Do not assert Seq length and then items separately, assert the Seq itself
- Do not assert Either with match, assert the side you expect
