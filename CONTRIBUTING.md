# Contributing to simple-fatfs

Before moving to the guidelines, I'd like to thank you for contributing to simple-fatfs

## Finding a bug

If you have found a bug, open an issue. Describe the bug with as much detail
as you can and how it differs from expected behaviour. Include the code that
triggered that bug and its stack backtrace if the bug caused your program to panic.

## Suggesting a feature

If the feature you want to suggest is simple, open an issue. If you believe
that is may need to be discussed beforehands, open a discussion.

## Submitting changes

- Try to follow [Conventional Commits](https://www.conventionalcommits.org) when commiting
- Please open only 1 PR per suggested change/feature. If there are multiple features
  you wanna add, open 1 PR for each one of them.
- Don't duplicate code: if there is a function that already does what you want, use it
- Try to keep your code organized: if you feel like it should belong to a different file,
  do it and if the consensus turns out to be that it shouldn't, we will simply just
  make an additional commit on your PR enforcing that before merging
- Follow the lints (exception: ExFAT data structures are a part of the codebase
  but no currently in use)
