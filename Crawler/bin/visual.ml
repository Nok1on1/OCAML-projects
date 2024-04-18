type 'a some = Some of 'a;;
type 'a lis = [] | ( :: ) of 'a * 'a lis * 'a some;;
let x = [1; 3; 2; 4; Some 3; Some 3; Some 3; Some 3];;



