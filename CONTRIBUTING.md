# Contributing to slFtp

We'd love for you to contribute to our source code and to make slFtp even better than it is today! <br ><br >
Here are the guidelines we'd like you to follow:

* [Git Commands](#aHelpGit)
* [Issues and Bugs](#issuesNbugs)
* [Commit Message Guidelines](#commitGuidelines)
* [Coding Guidelines](#codingGuidelines)
* [Semantic Versioning](http://semver.org/spec/v2.0.0.html)

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

Add your (single) changes and commit:
```shell
git diff
git add <files>
git commit
git push origin <feature-branch>
``` 

Updating an outdated branch with latest development branch:
  - rebase should be favoured over merge to avoid useless merge commits in history
```shell
git checkout dev
git pull
git checkout mybranch
git rebase dev
git push --force-with-lease origin mybranch
``` 

[anyway, here is git for dummies](https://www.mediawiki.org/wiki/Git_for_dummies)
or
[the full documentaion](https://git-scm.com/docs/)

<a name="issuesNbugs"></a> Issues and Bugs
-----
If you find a bug in the source code or a mistake in the documentation, help or config files, you can help us by submitting an issue or creating a merge-request with the proposed changes. <br>

<a name="commitGuidelines"></a> Commit Message Guidelines
-----
Each commit message consists of a <b>header</b>, a <b>body</b> and a <b>footer</b>. The header has a special format that includes a <b>type</b>, a <b>scope</b> and a <b>subject</b>:
```
<type>(<scope>): <subject>  <-- <header>
<BLANK LINE>
<body>
<BLANK LINE>
<footer>
```

The <b>header</b> is mandatory and the <b>scope</b> of the header is optional.

Any line of the commit message cannot be longer 100 characters! This allows the message to be easier to read on GitHub/GitLab as well as in various git tools.

## Revert

If the commit reverts a previous commit, it should begin with ```revert```, followed by the header of the reverted commit. <br>
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
* <b>update</b>: An update of e.g. external library files

## Scope

The scope could be anything specifying main changes of the commit.
Examples:
- non-specific changes in unit:
```kb, precatcher, rules, irccommands, ...```
- specific changes to classes:
```TSites, TPazo, TMP3Release, TQueueThread, ...```
- specific changes to functions:
```RemovePazoMKDIR, ProcessRequest, IsKnownGroup, FindIrcChannelSettings, ...```

## Subject

The subject contains succinct description of the change:

* use the imperative, present tense: "change" not "changed" nor "changes"
* no dot (.) at the end

## Message body

* just as in <subject> use imperative, present tense: “change” not “changed” nor “changes”
* includes motivation for the change and contrasts with previous behavior

## Footer 

The footer should contain any information about Breaking Changes and is also the place to reference GitLab issues, merge-requests or other commits for cross-references.
NOTE:
All breaking changes have to be mentioned as a breaking change block in the footer, which should start with the word ```BREAKING CHANGE:``` with a space or two newlines. <br>
The rest of the commit message is then the description of the change, justification and migration notes.

### Referencing issues

Closed bugs should be listed on a separate line in the footer prefixed with "Closes" keyword like this:

```Closes #234 ```

or in case of multiple issues:

```Closes #123, #245, #992```

<a name="codingGuidelines"></a> Coding Guidelines
-----
- General
  - the name of the variable should express its meaning/use/function
  - better write several small specific functions instead of one big function
  - try to avoid code duplications
  - use [generic classes](http://docwiki.embarcadero.com/Libraries/Rio/en/System.Generics.Collections) over old non-generic classes
  - <b>don't hack something into it -> refactor it and write proper code</b>
  - tests should be written if a function gets changed to check if it behaves the same after the change
- Comments
  - comments have to be written in [PasDoc](https://github.com/pasdoc/pasdoc/wiki) style
  - variables and functions in `interface` section <b>MUST</b> be documented
  - other comments could be made if useful (could be made in PasDoc style but not enforced)
- Global variables
  - use with care - only if it is REALLY needed
  - variables which are defined in the `interface` section and thus available in all units which use/include this unit
  - should start with `Gl` followed by an uppercased character
```
  GlSkiplistRegex: TRegExpr;
  GlMainConfig: TConfig;
```
- Unit _global_ variables
  - variables which are defined in the `implementation` section and thus available everywhere in that unit file
  - should start with `gl` followed by an uppercased character
```
  glLastAddtvmazeIDs: TList<String>;
  glSQLite3Lock: TCriticalSection = nil;
```
- Variables in classes/records/etc
  - should start with a capital `F` and the following character should also be uppercased
  - be made `private` if possible
  - if it needs to be public accessible do it with a `property` (write access only if really needed!)
```
  TIrcChannelSettings = class
  private
    FNetname: String; //< netname of IRC network
    FChannel: String; //< IRC channelname
    FInviteOnly: Boolean; //< @true if channel is invite only (you have to invite yourself first), @false otherwise
    ...
    property Netname: String read FNetname;
    property Channel: String read FChannel;
    property InviteOnly: Boolean read FInviteOnly write FInviteOnly;
    ...
```
- Functions
  - function name should describe the main functionality of the code
  - better write a few shorter functions instead of a huge one as it makes it complicated to follow everything
  - name of functions which exist in `implementation` part only should begin with `_`
    - could also be inside a function itself if it e.g. helps to reduce code duplications
    ```
    function _findMP3GenreOnAnnounce(const text: String; ts_data: TStringList): String;
    procedure _AnnounceConfigValue;
    ```
- Variables in functions
  - local variables should start with a lowercase `f` while the next character is uppercased
  - index variables can still be called `i`, `j`, etc
```
function TMyIrcThread.ShouldJoinGame: Boolean;
var
  i: Integer;
  fChanSettingsObj: TIrcChannelSettings;
  ...
```
- Parameters to functions
  - parameters should start with a lowercase `a` while the next character is uppercased
  - `const` should be used whenever useful to help the compiler to generate faster code
```
    { Creates a new TIrcChannelSettings entry which holds infos about Chankey, Chanroles and if channel is invite only
      @param(aNetname irc network name)
      @param(aChannel irc channel name)
      @param(aChanRoles irc chanroles for this channel)
      @param(aChankey channel key to join channel)
      @param(aInviteOnly @true if channel can only joined with previous invite, @false otherwise) }
    constructor Create(const aNetname, aChannel, aChanRoles: String; aChankey: String = ''; aInviteOnly: Boolean = True);
```
