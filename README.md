# Reverting a Pull Request

You can run this comment and than select the pull request you need to revert.

```bash
$ revert-pr
```

You can also provide a pull request number directly.

```bash
$ revert-pr 1234
```

You will be shown the list of all commits in the pull request, by pressing
`ctrl-a` you can select all of the commits or use `tab` to select only a
subset.

The tool will create one commit containing all the changes.
