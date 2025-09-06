FILENAME=fleetls_wasm.wasm

RELEASE_PATH=../target/wasm32-unknown-unknown/wasm-release/
DEBUG_PATH=../target/wasm32-unknown-unknown/debug/

RELEASE_FILE=$(RELEASE_PATH)/$(FILENAME)
DEBUG_FILE=$(DEBUG_PATH)/$(FILENAME)

OUT_PATH=../web-demo/assets/wasm
OUT_FILENAME=$(basename $(FILENAME))_bg.wasm
OUT_FILE=$(OUT_PATH)/$(OUT_FILENAME)

.PHONY: release debug clean clean-cargo


debug: $(DEBUG_FILE)
	mkdir -p $(OUT_PATH)
	wasm-bindgen $(DEBUG_FILE) --out-dir $(OUT_PATH) --target web --typescript --keep-debug

release: $(RELEASE_FILE)
	mkdir -p $(OUT_PATH)
	wasm-bindgen $(RELEASE_FILE) --out-dir $(OUT_PATH) --target web --typescript
	wasm-opt -Oz -o $(OUT_FILE).opt $(OUT_FILE)
	mv $(OUT_FILE).opt $(OUT_FILE)

$(RELEASE_FILE): FORCE
	cargo build --profile wasm-release
$(DEBUG_FILE): FORCE
	cargo build --profile dev

FORCE:

clean:
	rm -fr $(OUT_PATH)

clean-cargo: clean
	cargo clean
