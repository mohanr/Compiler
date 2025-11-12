
type 'a pair = {
  car : 'a ;
  cdr : 'a
}


let create =
    let bytes = Bytes.make 0 (Char.chr 0) in
    let pair = {car = bytes; cdr = bytes } in
    CCArray.make 1280 pair

let text = create

let cons x y  =
  {car = x; cdr = y}
