//
// F# image processing functions.
//
// More details?
//
// Shayan Rasheed
// University of Illinois
// 4/20/2022
//

namespace ImageLibrary

module Operations =
  //
  // all functions must be indented
  //

  //
  // Grayscale:
  //
  // Converts the image into grayscale and returns the 
  // resulting image as a list of lists. Conversion to 
  // grayscale is done by averaging the RGB values for 
  // a pixel, and then replacing them all by that average.
  // So if the RGB values were 25 75 250, the average 
  // would be 116, and then all three RGB values would 
  // become 116 — i.e. 116 116 116.
  //
  // Returns: updated image.
  //
  let rec avgList (img:(int*int*int) list) newImg =
    match img with
    | [] -> List.rev newImg
    | (a, b, c)::tail -> 
                        let avg = (a + b + c) / 3
                        avgList tail ((avg, avg, avg)::newImg)

  let rec _Grayscale (image:(int*int*int) list list) newImage =
    match image with
    | [] -> List.rev newImage
    | head::tail -> _Grayscale tail ((avgList head [])::newImage)

  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    _Grayscale image []


  //
  // Threshold
  //
  // Thresholding increases image separation --- dark values 
  // become darker and light values become lighter. Given a 
  // threshold value in the range 0 < threshold < color depth,
  // each RGB value is compared to see if it's > threshold.
  // If so, that RGB value is replaced by the color depth;
  // if not, that RGB value is replaced with 0. 
  //
  // Example: if threshold is 100 and depth is 255, then given 
  // a pixel (80, 120, 160), the new pixel is (0, 255, 255).
  //
  // Returns: updated image.
  //
  let replace (img:int) threshold depth =
    if img > threshold then
      depth
    else 
      0

  let rec calc img threshold depth newImg =
    match img with
    | [] -> List.rev newImg
    | (a, b, c)::tail -> 
                        let tuple = ((replace a threshold depth), (replace b threshold depth), (replace c threshold depth))
                        calc tail threshold depth ((tuple)::newImg)

  let rec _Threshold image threshold depth newImage =
    match image with
    | [] -> List.rev newImage
    | head::tail -> _Threshold tail threshold depth ((calc head threshold depth [])::newImage)

  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
    _Threshold image threshold depth []


  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
  let rec _FlipHorizontal image newImage =
    match image with
    | [] -> List.rev newImage
    | head::tail -> _FlipHorizontal tail ((List.rev head)::newImage)

  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    _FlipHorizontal image []


  //
  // Zoom:
  //
  // Zooms the image by the given zoom factor, which is an 
  // integer 0 < factor < 5. The function uses the nearest 
  // neighbor approach where each pixel P in the original 
  // image is replaced by a factor*factor block of P pixels.
  // For example, if the zoom factor is 4, then each pixel 
  // is replaced by a 4x4 block of 16 identical pixels. 
  // The nearest neighbor algorithm is the simplest zoom 
  // algorithm, but results in jagged images.
  //
  // Returns: updated image.
  //
  let rec helper2 newImg newList factor =
    match factor with
    | 0 -> newList
    | _ -> helper2 newImg (newImg::newList) (factor-1)

  let rec helper img newImg factor =
    match img with
    | [] -> helper2 newImg [] factor
    | head::tail -> helper tail (List.append newImg (List.replicate factor head)) factor

  let rec _Zoom image newImage factor =
    match image with
    | [] -> newImage
    | head::tail -> _Zoom tail (List.append newImage (helper head [] factor)) factor

  let rec Zoom (width:int)
               (height:int)
               (depth:int)
               (image:(int*int*int) list list)
               (factor:int) = 
    _Zoom image [] factor


  //
  // RotateRight90:
  //
  // Rotates the image to the right 90 degrees.
  //
  // Returns: updated image.
  //
  let rec _rotate image newList index =
    match image with
    | [] -> newList
    | head::tail -> _rotate tail ((List.item index head)::newList) index

  let rec rotate image newImage index width =
    if index = width then
      List.rev newImage
    else
      let newList = _rotate image [] index
      rotate image (newList::newImage) (index + 1) width

  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    // for now, just return the image back, i.e. do nothing:
    rotate image [] 0 width

