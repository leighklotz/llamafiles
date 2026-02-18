# Project History: LLM Help CLI for Linux and Mac

This document outlines the development history of the LLM Help CLI, providing an overview of key features, subsystems, and the evolution of the project based on the commit log.

The development of this project has followed a pattern of building core functionality (LLM interaction), expanding usability through shell scripts, extending it to an Emacs environment, and then refining. The evolution shows a clear effort to create a flexible, customizable tool that provides helpful assistance to developers and system administrators. Current development efforts seem to be focused on streamlining and optimizing the existing features.

The project has moved from initial proof-of-concept functionality to a more comprehensive tool with a range of integrated features. The shift towards removing unused features and simplifying interaction indicates the effort to produce a more mature and streamlined tool.

## I. Overview

The project started as a utility to provide help, summarization, and coding assistance via Large Language Models (LLMs) for Linux and Mac systems, using `llama.cpp` and `llamafiles` one-shot inference executables. It has evolved to integrate with various LLM backends, enhance usability through Bash shell scripting and Emacs integration (llm_el), and refine the overall user experience.  The core functionality revolves around prompt construction, API interaction, and output processing. The development is iterative, with a focus on improving prompting, error handling, and user configuration.

## II. Major Themes and Subsystems

Here's a breakdown of the major development areas, grouped by theme:

### A. Core LLM Interaction & API Handling (Jan 30 - Feb 18, 2025)

*   **API Abstraction:** Initial work focused on establishing a robust API interaction layer (`scripts/llm.sh`) to communicate with different LLM providers. The project started to support `llama.cpp` and `llamafiles` and has the architecture to support other providers. Protocols have been added such as authentication, request formatting, and error handling (`AUTHORIZATION_PARAMS`, `TEMPLATE_SETTINGS`).  This included setting up environment variables for configuration (`env.sh`).
*   **Prompt Engineering & Formatting:**  Significant effort was dedicated to crafting effective prompts to guide the LLM.  This includes experimental work with using system messages (`SYSTEM_MESSAGE`) and exploring contrastive prompts.
*	**Temporary File Management**: A system for registering and cleaning up temporary files during LLM interactions was introduced to improve script robustness.
*   **Verbose Logging & Debugging**: Development incorporated enhanced logging and debug capabilities, making it easier to trace execution and diagnose issues.

### B. Shell Scripting Utilities (Mar 3 - Mar 13, 2025)

*   **Help & Information Scripts:**
    *   `help.sh`: The primary CLI for general Linux help.
    *   `ask.sh`: A variant focused on generic questions.
    *   `machelp.sh`: CLI tailored for Mac-specific assistance.
    *   `summarize.sh`:  For summarizing text from URLs or standard input.
*   **System Information & Utilities:**
    *   `nvfree.sh`: For checking GPU usage.
    *   `systype.sh`: To provide context for distro-specific questions. Code completion, extraction, styling and rewriting features were also introduced
    *   Added `unblock.sh` and `catfiles.sh` scripts for file manipulation and display.
*   **Command Output Processing:**
    *   `codeblock.sh`, `bashblock.sh`:  Wrapping command outputs in code blocks for better readability and integration with LLMs.

### C. Emacs Integration (Mar 10-13, 2025)

*   **`llm_el` Package:** The development of an Emacs package (`llm_el`) for tighter integration with the LLM functionality.
*   **Key Bindings & Functionality:** Adding numerous key bindings for tasks like code rewriting, summarization, and information retrieval directly within Emacs.
*   **Diff Integration:** Enabling diff-based code analysis and merging using the LLM.
*	**llm-keystroke-npc.el**: a test program for keystroke suggestions and editing help.

### D. Refactoring and Clean-Up (Mar 13 - Sep 6, 2025)

*   **Code Organization:** Restructuring the code base, removing redundant logic, and improving overall maintainability.
*   **Documentation**: Improving documentation as the tool matures.
*   **Feature Removal:** Removing deprecated or unused features.
*	**Error Handling and improved configuration file loading**.

## III. Significant Features & Milestones

*   **Initial LLM Integration (Jan 30 - Feb 18, 2025):** Establishing the core API interaction with LLM providers.
*   **Shell Scripting Suite (Feb 18 - March 3, 2025):** Developing a set of command-line tools for various use cases.
*   **Emacs Lisp Integration (Mar 10 - 13, 2025):** offering a highly integrated workflow within the Emacs editor.
*   **Improved Prompting and System Messages (Ongoing):** Refinements to prompt construction to improve the quality of LLM responses.
*	**Downlink with Playwright:** The implementation of Playwright/headless chrome to fetch content from webpages and enable conversion to markdown.
*   **Removal of llamafile support & optimization of API calls (Sep 6, 2025)**
