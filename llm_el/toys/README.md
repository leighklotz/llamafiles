# llm-keystroke-npc

This Emacs Lisp package implements a simple "NPC" (Non-Player Character) that observes your Emacs keystrokes and uses an LLM to suggest improvements to your Emacs usage.

## Description

`llm-keystroke-npc.el` defines an Emacs function `m-x llm-keystroke-npc` that:

*   Periodically (every 30 seconds by default) captures the last 100 keystrokes.
*   Stores these keystrokes in a temporary buffer (`*llm-keystroke-buffer*`).
*   Uses the `llm-ask` function (assumed to be provided by another package - see Dependencies) to prompt an LLM with the keystrokes and ask for suggestions.
*   Displays any LLM suggestions in a message buffer.

This package aims to provide an interactive way to learn more efficient Emacs workflows by leveraging the power of large language models.  It's a toy project intended for experimentation and fun.

## Dependencies

*   **Emacs:** Requires Emacs 27 or later.
*   **`llm-ask` function:** This package assumes the existence of a function called `llm-ask` which handles communication with a Large Language Model. You will need to provide or install a package that defines `llm-ask`.  The expected signature is `(llm-ask prompt start end)`, where `prompt` is a string and `start` and `end` are buffer positions.  Adapt the code if your `llm-ask` function has a different signature.

## Installation

1.  Clone or download the `llm-keystroke-npc.el` file.
2.  Place the file in your Emacs load path (e.g., `~/.emacs.d/lisp/`).
3.  Evaluate the buffer with `M-x load-file RET /path/to/llm-keystroke-npc.el RET`.
4.  Alternatively, add `(load-file "/path/to/llm-keystroke-npc.el")` to your `init.el` or `.emacs` file.

## Usage

1.  **Set up `llm-ask`:**  Ensure the `llm-ask` function is defined and can successfully communicate with your chosen LLM provider.
2.  **Enable the NPC:** After loading the package, the NPC will automatically start running.  It will capture keystrokes as you type, and after 30 seconds, it will display any LLM suggestions it receives.
3.  **Customize:**
    *   `llm-keystroke-count`:  Change the number of keystrokes to capture.
    *   `llm-capture-buffer-list`: Add buffers to the list where keystrokes will be captured. Use `M-x llm-npc-watch-buffer` while in the buffer you want to monitor.
4.  **Disable the NPC:**  To temporarily disable the NPC, set the variable `inhibit-llm-keystroke-npc` to a non-nil value.
5.  **Manual Execution:** You can manually trigger the NPC by evaluating `(llm-keystroke-npc)`.

## Key Variables

*   `llm-keystroke-buffer`:  The name of the buffer used to store keystrokes.
*   `llm-keystroke-list`:  The list containing the captured keystrokes.
*   `llm-keystroke-timer`: The timer used to run the NPC periodically.
*   `llm-keystroke-count`: The number of keystrokes to capture before generating suggestions.
*   `inhibit-llm-keystroke-npc`:  Disables the NPC when set to a non-nil value.
*   `llm-capture-buffer-list`: A list of buffers to monitor for keystrokes.

## Future Enhancements

*   Add more sophisticated keystroke analysis.
*   Allow users to customize the prompt sent to the LLM.
*   Integrate with Emacs advice to conditionally capture keystrokes.
*   Provide a UI to manage the NPC settings.
*   Cache LLM responses to improve performance.
*   Handle errors from the `llm-ask` function more gracefully.
*   Support different LLM providers.

## License

This code is released under the [MIT License](LICENSE).
