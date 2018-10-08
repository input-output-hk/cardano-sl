# How to merge PRs with Bors

[Bors](https://bors.tech/) is a GitHub bot that queues merging and
testing of PRs to prevent breaking code merges from reaching the
`develop` branch. These

It works by sending commands in messages on GitHub pull requests.

On the [Bors website](https://bors.tech/) this is a [reference
documentation page](https://bors.tech/documentation/) which lists all
the possible commands you can use.

## Before merging

Before merging, your PR must have:

- at least one review approval
- no review rejections
- green CI status

## How to merge

Add a comment to the pull request with the text `bors r+`.

Bors will handle things from there. It merges your PR into a staging
branch and then waits for that branch to pass CI before merging to
`develop`. If something fails, it will send a message to the PR saying
so.

## Who can merge

Anyone with push access to the GitHub repository can send commands to
`iohk-bors`.

## Hosting

IOHK host our own Bors bot called `iohk-bors`.

The web interface is at https://bors-ng.aws.iohkdev.io/. Generally,
you don't need the web interface to merge PRs.

If using the web interface, ensure that you are using ours, rather
than the one hosted by the Bors developers at https://app.bors.tech/.

## Configuration

Most of the settings for Bors are in [`bors.toml`](./../../bors.toml).

The settings for branches and permissions are found in the web
interface at https://bors-ng.aws.iohkdev.io/repositories .
