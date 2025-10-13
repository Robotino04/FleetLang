import subprocess
import time
import json
import sys
from pathlib import Path


def run_binary(binary_path, runs=10, output_file="runtimes.json"):
    binary = Path(binary_path)
    if not binary.is_file():
        print(f"Error: Binary '{binary_path}' not found.")
        sys.exit(1)

    runtimes = []

    for i in range(runs):
        print(f"Running iteration {i + 1}/{runs}...")
        start = time.time()
        try:
            subprocess.run(
                [str(binary)],
                check=True,
                #stdout=subprocess.DEVNULL,
                #stderr=subprocess.DEVNULL,
            )
        except subprocess.CalledProcessError as e:
            print(f"Run {i + 1} failed: {e}")
        end = time.time()
        duration = end - start
        runtimes.append(duration)

    # Save to JSON file
    with open(output_file, "w") as f:
        json.dump(runtimes, f, indent=4)

    print(f"\nFinished {runs} runs. Results saved to '{output_file}'.")


# Example usage:
if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description="Run a binary multiple times and record execution times."
    )
    parser.add_argument("binary_path", help="Path to the binary executable")
    parser.add_argument(
        "--runs", type=int, default=10, help="Number of times to run the binary"
    )
    parser.add_argument(
        "--output", default="runtimes.json", help="Output JSON file to store runtimes"
    )

    args = parser.parse_args()
    run_binary(args.binary_path, args.runs, args.output)
