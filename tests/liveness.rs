// To make sure what the right answer is :)
fn live_var_stress_rs() -> i32 {
    let a: i32 = 1;
    let b: i32 = a + 2;
    let c: i32 = b * 3;
    let d: i32 = c - a;
    let e: i32 = d + b;
    let f: i32 = e * 2;
    let g: i32 = f / 3;
    let h: i32 = g + (a + b) + c;
    let i: i32 = h - d;
    let j: i32 = i + e;
    let k: i32 = j * 2 - f;
    let m: i32 = k + g + h;
    let n: i32 = m / (2 + a);
    let o: i32 = n + m + k;
    let p: i32 = o - n + j;
    let q: i32 = p * 2 + i;
    let r: i32 = q - o + h;
    let s: i32 = r + p - g;
    let t: i32 = s + q - f;
    let u: i32 = t + r - e;
    let v: i32 = u + s - d;
    let w: i32 = v + t - c;
    let x: i32 = w + u - b;
    let y: i32 = x + v - a;
    let z: i32 = y + w + x;

    let mut acc: i32 = z;
    let mut idx: i32 = 0;

    while idx < 5 {
        let inner: i32 = acc + idx;
        acc = acc + inner - (idx * 2) + (idx + a);
        if (idx - 2) == 0 {
            let shadow: i32 = acc + y;
            acc = shadow - inner + (idx + b);
        } else {
            let shadow: i32 = acc - x;
            acc = shadow + inner - (idx + c);
        }
        idx += 1;
    }

    let branch_a: i32 = if acc > 0 {
        let tmp: i32 = acc + d + e;
        tmp - f + g
    } else {
        let tmp: i32 = acc - h - i;
        tmp + j - k
    };

    let branch_b: i32 = if branch_a < acc {
        let tmp: i32 = branch_a + v;
        tmp + acc
    } else {
        let tmp: i32 = branch_a - acc;
        tmp - acc
    };

    let l: i32 = branch_a / 2 + m;
    let branch_c: i32 = branch_b + l + n;
    let final_acc: i32 = acc + branch_a + branch_b + branch_c + o + p + q + r + s + t + u + v + w + x + y + z;

    final_acc - (l / 2)
}