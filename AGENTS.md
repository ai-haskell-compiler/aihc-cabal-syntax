# Repository instructions

Apply these rules to all work in this repository.

## Commits and pull requests

- Use Conventional Commits for every commit message and pull request title.
- Use this format: `<type>[optional scope][!]: <description>`.
- Select a type that identifies the change, such as `feat`, `fix`, `docs`, `test`, `build`, `ci`, or `chore`.
- Write a short description that starts with an imperative verb.
- For a breaking change, add `!` before the colon or a `BREAKING CHANGE:` footer.
- Explain the breaking change and the necessary migration steps.
- Example: `docs: add repository instructions`.
- In the pull request description, explain the change and give the results of the checks.
- Identify checks that you could not run. Give the reason.

## Language

- Use ASD-STE100 Simplified Technical English for all text that you write or change.
- Apply this rule to documentation, comments, interface text, messages, commit text, pull requests, and task responses.
- Follow the writing rules and dictionary in the [official ASD-STE100 specification](https://asd-ste100.org/).
- Use approved words with their approved meanings and parts of speech.
- Use technical names and technical verbs only as permitted by the specification.
- Use the same term for the same concept.
- Use short sentences and active voice. Give one instruction in each sentence.
- Use direct instructions. Avoid idioms and unnecessary words.
- Keep required code syntax, identifiers, commands, and Conventional Commits markers exact.
- Check the text against the specification. Clear English alone does not show compliance.

## Reproducible builds and tests

- Use Nix for all project builds and tests, locally and in continuous integration.
- Keep the build and test environment in version control.
- Use a `flake.nix` file and commit its `flake.lock` file.
- Pin all external inputs and dependencies. Include compilers, build tools, and test tools.
- Do not depend on tools or libraries from the host system.
- Do not use unpinned downloads, local absolute paths, or undeclared environment variables.
- Keep network access out of build and test steps. Declare dependencies through Nix before those steps.
- Make tests independent of the current time and external services. Fix random seeds where applicable.
- Provide build outputs for `nix build` and test checks for `nix flake check`.
- Run `nix build --no-update-lock-file` and `nix flake check --no-update-lock-file` before you submit code changes.
- Use the same commands in continuous integration.
- Use `nix develop` for development tools. Tests must also run through Nix checks without an interactive shell.
- Change dependency pins only when the task requires it. Explain each update in the pull request.
- If the Nix configuration is absent, add it with the first change that needs a build or test.
- If a required check cannot run, report the cause. Do not report an unperformed check as successful.
