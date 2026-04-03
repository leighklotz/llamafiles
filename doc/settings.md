# User-Settable Parameters and Environment Variables

## Generation & Model Settings
| Env Var             | CLI Flag             | Default    | Description                                  |
|:--------------------|:---------------------|:-----------|:---------------------------------------------|
| `INFERENCE_MODE`    | `--inference-mode`   | `instruct` | Mode of inference (e.g., instruct, chat).    |
| `TEMPERATURE`       | `--temperature`      | ''         | Sampling temperature.                        |
| `N_PREDICT`         | `--n-predict`        | ''         | Maximum number of tokens to generate.        |
| `SEED`              | N/A                  | `NaN`      | Random seed for generation.                  |
| `REPEAT_PENALTY`    | N/A                  | `1`        | Penalty for repeating tokens.                |
| `PENALIZE_NL`       | N/A                  | `false`    | Whether to penalize newlines.                |
| `REASONING_EFFORT`  | `--reasoning-effort` | `low`      | Effort level for reasoning models.           |
| `REASONING_BUDGET`  | N/A                  | `2048`     | Token budget for reasoning.                  |
| `ENABLE_THINKING`   | `--enable-thinking`  | `false`    | Enable "thinking" capabilities in the model. |
| `TOP_K`             | N/A                  | `20`       | Top-K sampling limit.                        |
| `TOP_P`             | N/A                  | `0.95`     | Nucleus sampling probability.                |
| `MIN_P`             | N/A                  | `0.1`      | Minimum probability for sampling.            |
| `TYPICAL_P`         | N/A                  | `1.000`    | Typical sampling probability.                |
| `REPEAT_LAST_N`     | N/A                  | `1024`     | Window size for repetition penalty.          |
| `FREQUENCY_PENALTY` | N/A                  | `0.000`    | Penalty based on token frequency.            |
| `PRESENCE_PENALTY`  | N/A                  | `0.000`    | Penalty based on token presence.             |

## Prompt & Context Settings
| Env Var           | CLI Flag         | Default | Description                                                 |
|:------------------|:-----------------|:--------|:------------------------------------------------------------|
| `SYSTEM_MESSAGE`  | N/A              | ''      | The system prompt provided to the LLM.                      |
| `USE_SYSTEM_ROLE` | N/A              | ''      | If empty, system message is prepended to the user question. |
| `USE_GRAMMAR`     | N/A              | ''      | Whether to apply a GBNF grammar file.                       |
| `GRAMMAR_FILE`    | `--grammar-file` | ''      | Path to the `.gbnf` grammar file.                           |

## API & Connection Settings
| Env Var               | CLI Flag | Default                 | Description                                               |
|:----------------------|:---------|:------------------------|:----------------------------------------------------------|
| `VIA_API_CHAT_BASE`   | N/A      | `http://localhost:5000` | Base URL for the API server.                              |
| `OPENAI_API_KEY`      | N/A      | ''                      | API key; triggers Bearer auth and GPT-style templates.    |
| `LLAMA_SERVER_MODEL`  | N/A      | ''                      | Specifies the model name to send in the API request.      |
| `MODEL_NAME_OVERRIDE` | N/A      | `None`                  | Manual override for the model name during identification. |

## Execution & Logging Settings
| Env Var                    | CLI Flag           | Default         | Description                                               |
|:---------------------------|:-------------------|:----------------|:----------------------------------------------------------|
| `INFO`                     | `--info`           | `VERBOSE`       | Enable info-level logging.                                |
| `VERBOSE`                  | `--verbose` / `-v` | ''              | Enable verbose logging.                                   |
| `DEBUG`                    | `--debug`          | ''              | Enable debug logging and `set -x`.                        |
| `DEBUG_SHOW_JSON`          | N/A                | ''              | Print the raw JSON API response.                          |
| `LOG_DISABLE`              | ''                 | `--log-disable` | Set to `" "` (space) to override default logging disable. |
| `KEEP_PROMPT_TEMP_FILE`    | N/A                | `ALL`           | Cleanup policy: `NONE`, `ERROR`, or `ALL`.                |
| `DO_STDIN`                 | `--stdin` / `-i`   | N/A             | Whether to read input from STDIN.                         |
| `PROCESS_QUESTION_ESCAPES` | `-e`               | ''              | Process literal `\n` as newlines in the question.         |
| `RAW_FLAG`                 | `--raw-input`      | ''              | Flag to indicate raw input processing.                    |
