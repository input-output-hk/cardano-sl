---
name: ADR
about: Document an Architectural Decision

---

# Context

<!-- WHEN PROPOSED
What is the issue that we are seeing that is motivating this decision or change. 
Give any elements that help understanding where this issue comes from. Leave no 
room for suggestions or implicit deduction.
-->


# Decision

<!-- WHEN PROPOSED
Give details about the architectural decision and what it is doing. Be
extensive: use schemas and references when possible; do not hesitate to use
schemas and references when possible.
-->


# Acceptance Criterias

<!-- WHEN PROPOSED
Use standard vocabulary to describe requirement levels RFC-2119: Must-Should-May.

e.g. 

1. The API _must_ support creation of wallets through a dedicated endpoint.
-->




---

# Development Plan

<!-- WHEN IN PROGRESS 
In the form of a TODO list, explain how the ticket is going to be tackled and how
you intend to proceed. 

e.g.

- [ ] I intend to extend the existing handlers and use the wallet layer to implement 
  the necessary steps.

- [ ] I plan on testing the endpoint by adding a few integration scenarios
-->


# PR

<!-- WHEN IN PROGRESS
List of all PRs related to this ticket.

e.g.

| Number | Base            |
| ---    | ---             |
| #14    | `develop`       |
| #42    | `release/2.0.0` |
-->

| Number   | Base      |
| ---      | ---       |
| #?       | `develop` |


# QA 

<!-- WHEN IN PROGRESS
How are we covering acceptance criteria. Give here manual steps or tests that
are covering the above criterias. 

e.g.

| Criteria | Coverage |
| ---      | ---      |
| 1.       | The creation has been covered has part of testing (cf Scenario/Transactions, tests prefixed with #14) | 

-->

| Criteria | Coverage |
| ---      | ---      |
| ?        | -        |





---

# Retrospective

<!-- WHEN CLOSED
Looking back on the ticket, what went wrong with the initial development plan? Were there any 
unforeseen difficulties? Did anything popped out during development that may require investigation
or special attention? Anything we can do moving forward?

e.g.
- Turns out the wallet layer wasn't implemented at all so this has to be done as an extra step. 
- Integration tests are running but now takes an unexpected long time. I'll open a ticket to investigate 
  this regression.
-->
