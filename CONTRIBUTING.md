# Contributing to slFtp

We'd love for you to contribute to our source code and to make slFtp even better than it is today! <br ><br >
Here are the guidelines we'd like you to follow:

* [Git Commands](#aHelpGit)
* [Issues and Bugs](#issuesNbugs)
* [Commit Message Guidelines](#commitGuidelines)
* [Feature Requests](#featurereq)

* [Semantic Versioning] (http://semver.org/spec/v2.0.0.html)

<a name="aHelpGit"></a> Git Commands 
-----
 
Download the newest branches from remote:
```shell
git fetch
```
Download and Merge the newest branches from remote:
```shell
git pull
```
 
Download and Merge a specified branch:
```shell
git checkout branch_name
``` 

[anyway, here is a good documentaion](https://git-scm.com/docs/)

<a name="issuesNbugs"></a> Issues and Bugs
-----
If you find a bug in the source code or a mistake in the documentation, help or config files, you can help us by submitting an issue. <br>
please use the right Tags on the issuse.


<a name="commitGuidelines"></a> Commit Message Guidelines
-----
Each commit message consists of a <b>header</b>, a <b>body</b> and a <b>footer</b>. The header has a special format that includes a <b>type</b>, a <b>scope</b> and a <b>subject</b>:
```
<type>(<scope>): <subject>
<BLANK LINE>
<body>
<BLANK LINE>
<footer>
```

The <b>header</b> is mandatory and the <b>scope</b> of the header is optional.

Any line of the commit message cannot be longer 100 characters! This allows the message to be easier to read on GitHub as well as in various git tools.

## Revert

If the commit reverts a previous commit, it should begin with ```revert:```, followed by the header of the reverted commit. <br>
In the body it should say: ```This reverts commit <hash>.```, where the hash is the SHA of the commit being reverted.

## Type

<b>Must</b> be one of the following:

* <b>feat</b>: A new feature
* <b>fix</b>: A bug fix
* <b>style</b>: Changes that do not affect the meaning of the code (white-space, formatting, missing semi-colons, etc)
* <b>refactor</b>: A code change that neither fixes a bug nor adds a feature
* <b>perf</b>: A code change that improves performance
* <b>chore</b>: Changes to the build process or auxiliary tools and libraries such as documentation generation
* <b>remove</b>: Removal of code duplicates, deprecated functions, unused files, etc

## Scope

The scope could be anything specifying place of the commit change. For example
```kb, precatcher, rules, irccommands, etc...```
#### TODO: Create a list, best would be we use the Issuses-Tags?

## Subject

The subject contains succinct description of the change:

* use the imperative, present tense: "change" not "changed" nor "changes"
* don't capitalize first letter
* no dot (.) at the end

## Message body

* just as in <subject> use imperative, present tense: “change” not “changed” nor “changes”
* includes motivation for the change and contrasts with previous behavior

## Footer 

The footer should contain any information about Breaking Changes and is also the place to reference GitLab issues that this commit Closes.

### Message footer

All breaking changes have to be mentioned as a breaking change block in the footer, which should start with the word ```BREAKING CHANGE:``` with a space or two newlines. <br>
The rest of the commit message is then the description of the change, justification and migration notes. 

### Referencing issues

Closed bugs should be listed on a separate line in the footer prefixed with "Closes" keyword like this:

```Closes #234 ```

or in case of multiple issues:

```Closes #123, #245, #992```


### Examples


<a name="featurereq"></a> Feature Requests
-----




