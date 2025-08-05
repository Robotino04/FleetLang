use std::thread::Builder;

use crate::common::assert_compile_and_output_subprocess;

// equivalent to the fleet implementation
fn generate_mandelbrot_ppm(width: usize, height: usize) -> String {
    let num_iterations = 5000;
    let mut image = vec![vec![0.0f32; width]; height];

    // Compute Mandelbrot values
    for (py, row) in image.iter_mut().enumerate() {
        for (px, pixel) in row.iter_mut().enumerate() {
            let x0 = scale_x(px, width, height);
            let y0 = scale_y(py, height);

            let mut x = 0.0f32;
            let mut y = 0.0f32;
            let mut i = 0;

            while i < num_iterations && x * x + y * y <= 4.0f32 {
                let xtemp = x * x - y * y + x0;
                y = 2.0f32 * x * y + y0;
                x = xtemp;
                i += 1;
            }

            *pixel = i as f32 / num_iterations as f32;
        }
    }

    // Generate PPM (P3 format) string output
    let mut output = String::new();
    output.push_str("P3\n");
    output.push_str(&format!("{width} {height}\n"));
    output.push_str("255\n");

    for row in image.iter() {
        for &value in row.iter() {
            // make image binary to avoid float-rounding issues in test
            let intensity = if value != 0.0 { 255 } else { 0 };
            output.push_str(&format!("{intensity} {intensity} {intensity} "));
        }
        output.push('\n');
    }

    output
}

fn scale_x(x: usize, width: usize, height: usize) -> f32 {
    (x as f32 / width as f32 - 0.5f32) * 2.0f32 * width as f32 / height as f32
}

fn scale_y(y: usize, height: usize) -> f32 {
    (y as f32 / height as f32 - 0.5f32) * 2.0f32
}

#[test]
fn mandelbrot_set() {
    // Downscaled a bunch or the fleet version takes way too much stack space and the string
    // comparisons take ages. There is also still float-rounding issues, which may become an
    // issue if we have more pixels. This size happens to work with "only" 64MB of stack. If
    // testing ever hangs, it is probably this test trying and failing to print megabytes of
    // image data that doesn't match.
    let width = 1920 / 20;
    let height = 1080 / 20;

    let child_with_lots_of_memory = Builder::new()
        .stack_size(64 * 1024 * 1024)
        .spawn(move || {
            assert_compile_and_output_subprocess(
                &include_str!("mandelbrot_set.fl")
                    .replace("1920", &width.to_string())
                    .replace("1080", &height.to_string()),
                0,
                generate_mandelbrot_ppm(width, height),
                "",
            );
        })
        .unwrap();

    child_with_lots_of_memory.join().unwrap();
}
