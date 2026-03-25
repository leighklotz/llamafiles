# GLM-4.7-Flash-UD-Q6_K_XL.gguf
presets 'GLM-4.7-Flash.yaml'
./llama-server --model downloaded_models/GLM-4.7-Flash-UD-Q6_K_XL.gguf --port 11433 --host "0.0.0.0" -fa on --ctx-size 48000 --temp 0.7 --top-p 1.0 --min-p 0.01 --jinja -ngl 99


gemma-3-27b-it-Q4_K_M.gguf 
presets 'gemma-3.yaml'
llama-server --model user_data/models/gemma-3-27b-it-Q4_K_M.gguf --ctx-size 14320 --gpu-layers 63 --batch-size 2048 --ubatch-size 1024 --port 59769 --no-webui --flash-attn on --rope-freq-scale 0.125 --rope-freq-base 1000000 --mmproj user_data/mmproj/mmproj-google_gemma-3-27b-it-bf16.gguf


### TODO: Skeleton function `presets "$fn"`
### should convert newline file contents `name:value` pairs
### to double-dash bash CLI pairs: 
### ```
### # $ cat
### foo.config
### temperature: 0.7
### top_p: 0.8
### top_k: 20
### 
### --temperature 0.7 --top-k 0.8 --top_k 20
### ```
function presets() {
    fn="$1"
    cat "$fn"
}
