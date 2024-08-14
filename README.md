# colortransfer

`colortransfer` is a small tool that transfers the color distribution
of one image to another image. It offers two methods of color transfer,
histogram matching and ellipsoid transformation.

## Color Transfer Methods

### Histogram Matching

This method adjusts the mean and standard deviation of the color channels
in the input image to match those of the reference image. It's a simple
and effective approach for many cases, but it may not capture more
complex color relationships.

### Ellipsoid Transformation

The ellipsoid transformation method offers a more advanced approach to
color transfer:

- It models the color distributions of both the input and reference
  images as 3D ellipsoids in the color space.
- The method then computes a transformation that maps the input image's
  color ellipsoid to match the reference image's color ellipsoid.
- This transformation takes into account not just the mean and standard
  deviation, but also the covariance between color channels, allowing for
  a more nuanced color transfer.
- The result often preserves the relative relationships between colors
  better than simple histogram matching, potentially leading to more
  natural-looking results, especially for images with complex color
  distributions.

## Run

```shell
nix run github:damianfral/colortransfer -- \
  --input my-photo.jpg \
  --reference nice-colors.jpg \
  --output my-photo-with-nice-colors.jpg \
```

## Options

- `--input`: The image to be used as the color source
- `--reference`: The image we want to transform
- `--output`: The output image
- `--method`: The color transfer method to use (default: 2)
  - 1: Histogram matching
  - 2: Ellipsoid transformation

## Examples

| Input Image | Reference Image | Histogram Matching | Ellipsoid Transformation
|-------------|-----------------|--------------------|--------------------------
| ![Input](./test-resources/originals/ales-krivec-4miBe6zg5r0-unsplash.jpg) | ![Reference](./test-resources/originals/benjamin-voros-phIFdC6lA4E-unsplash.jpg) | ![HM](./test-resources/processed/ales-krivec-4miBe6zg5r0--benjamin-voros-phIFdC6lA4E--histogram-matching.jpeg) | ![ET](./test-resources/processed/ales-krivec-4miBe6zg5r0--benjamin-voros-phIFdC6lA4E--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/ales-krivec-4miBe6zg5r0-unsplash.jpg) | ![Reference](./test-resources/originals/jamie-fenn-XhzdJk1za2k-unsplash.jpg) | ![HM](./test-resources/processed/ales-krivec-4miBe6zg5r0--jamie-fenn-XhzdJk1za2k--histogram-matching.jpeg) | ![ET](./test-resources/processed/ales-krivec-4miBe6zg5r0--jamie-fenn-XhzdJk1za2k--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/ales-krivec-4miBe6zg5r0-unsplash.jpg) | ![Reference](./test-resources/originals/pawel-czerwinski-6lQDFGOB1iw-unsplash.jpg) | ![HM](./test-resources/processed/ales-krivec-4miBe6zg5r0--pawel-czerwinski-6lQDFGOB1iw--histogram-matching.jpeg) | ![ET](./test-resources/processed/ales-krivec-4miBe6zg5r0--pawel-czerwinski-6lQDFGOB1iw--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/ales-krivec-4miBe6zg5r0-unsplash.jpg) | ![Reference](./test-resources/originals/pine-watt-2Hzmz15wGik-unsplash.jpg) | ![HM](./test-resources/processed/ales-krivec-4miBe6zg5r0--pine-watt-2Hzmz15wGik--histogram-matching.jpeg) | ![ET](./test-resources/processed/ales-krivec-4miBe6zg5r0--pine-watt-2Hzmz15wGik--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/ales-krivec-4miBe6zg5r0-unsplash.jpg) | ![Reference](./test-resources/originals/ravi-sharma-f2h77fiLu18-unsplash.jpg) | ![HM](./test-resources/processed/ales-krivec-4miBe6zg5r0--ravi-sharma-f2h77fiLu18--histogram-matching.jpeg) | ![ET](./test-resources/processed/ales-krivec-4miBe6zg5r0--ravi-sharma-f2h77fiLu18--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/ales-krivec-4miBe6zg5r0-unsplash.jpg) | ![Reference](./test-resources/originals/tania-miron-EKX3Lx-t5CM-unsplash.jpg) | ![HM](./test-resources/processed/ales-krivec-4miBe6zg5r0--tania-miron-EKX3Lx-t5CM--histogram-matching.jpeg) | ![ET](./test-resources/processed/ales-krivec-4miBe6zg5r0--tania-miron-EKX3Lx-t5CM--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/benjamin-voros-phIFdC6lA4E-unsplash.jpg) | ![Reference](./test-resources/originals/ales-krivec-4miBe6zg5r0-unsplash.jpg) | ![HM](./test-resources/processed/benjamin-voros-phIFdC6lA4E--ales-krivec-4miBe6zg5r0--histogram-matching.jpeg) | ![ET](./test-resources/processed/benjamin-voros-phIFdC6lA4E--ales-krivec-4miBe6zg5r0--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/benjamin-voros-phIFdC6lA4E-unsplash.jpg) | ![Reference](./test-resources/originals/jamie-fenn-XhzdJk1za2k-unsplash.jpg) | ![HM](./test-resources/processed/benjamin-voros-phIFdC6lA4E--jamie-fenn-XhzdJk1za2k--histogram-matching.jpeg) | ![ET](./test-resources/processed/benjamin-voros-phIFdC6lA4E--jamie-fenn-XhzdJk1za2k--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/benjamin-voros-phIFdC6lA4E-unsplash.jpg) | ![Reference](./test-resources/originals/pawel-czerwinski-6lQDFGOB1iw-unsplash.jpg) | ![HM](./test-resources/processed/benjamin-voros-phIFdC6lA4E--pawel-czerwinski-6lQDFGOB1iw--histogram-matching.jpeg) | ![ET](./test-resources/processed/benjamin-voros-phIFdC6lA4E--pawel-czerwinski-6lQDFGOB1iw--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/benjamin-voros-phIFdC6lA4E-unsplash.jpg) | ![Reference](./test-resources/originals/pine-watt-2Hzmz15wGik-unsplash.jpg) | ![HM](./test-resources/processed/benjamin-voros-phIFdC6lA4E--pine-watt-2Hzmz15wGik--histogram-matching.jpeg) | ![ET](./test-resources/processed/benjamin-voros-phIFdC6lA4E--pine-watt-2Hzmz15wGik--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/benjamin-voros-phIFdC6lA4E-unsplash.jpg) | ![Reference](./test-resources/originals/ravi-sharma-f2h77fiLu18-unsplash.jpg) | ![HM](./test-resources/processed/benjamin-voros-phIFdC6lA4E--ravi-sharma-f2h77fiLu18--histogram-matching.jpeg) | ![ET](./test-resources/processed/benjamin-voros-phIFdC6lA4E--ravi-sharma-f2h77fiLu18--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/benjamin-voros-phIFdC6lA4E-unsplash.jpg) | ![Reference](./test-resources/originals/tania-miron-EKX3Lx-t5CM-unsplash.jpg) | ![HM](./test-resources/processed/benjamin-voros-phIFdC6lA4E--tania-miron-EKX3Lx-t5CM--histogram-matching.jpeg) | ![ET](./test-resources/processed/benjamin-voros-phIFdC6lA4E--tania-miron-EKX3Lx-t5CM--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/jamie-fenn-XhzdJk1za2k-unsplash.jpg) | ![Reference](./test-resources/originals/ales-krivec-4miBe6zg5r0-unsplash.jpg) | ![HM](./test-resources/processed/jamie-fenn-XhzdJk1za2k--ales-krivec-4miBe6zg5r0--histogram-matching.jpeg) | ![ET](./test-resources/processed/jamie-fenn-XhzdJk1za2k--ales-krivec-4miBe6zg5r0--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/jamie-fenn-XhzdJk1za2k-unsplash.jpg) | ![Reference](./test-resources/originals/benjamin-voros-phIFdC6lA4E-unsplash.jpg) | ![HM](./test-resources/processed/jamie-fenn-XhzdJk1za2k--benjamin-voros-phIFdC6lA4E--histogram-matching.jpeg) | ![ET](./test-resources/processed/jamie-fenn-XhzdJk1za2k--benjamin-voros-phIFdC6lA4E--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/jamie-fenn-XhzdJk1za2k-unsplash.jpg) | ![Reference](./test-resources/originals/pawel-czerwinski-6lQDFGOB1iw-unsplash.jpg) | ![HM](./test-resources/processed/jamie-fenn-XhzdJk1za2k--pawel-czerwinski-6lQDFGOB1iw--histogram-matching.jpeg) | ![ET](./test-resources/processed/jamie-fenn-XhzdJk1za2k--pawel-czerwinski-6lQDFGOB1iw--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/jamie-fenn-XhzdJk1za2k-unsplash.jpg) | ![Reference](./test-resources/originals/pine-watt-2Hzmz15wGik-unsplash.jpg) | ![HM](./test-resources/processed/jamie-fenn-XhzdJk1za2k--pine-watt-2Hzmz15wGik--histogram-matching.jpeg) | ![ET](./test-resources/processed/jamie-fenn-XhzdJk1za2k--pine-watt-2Hzmz15wGik--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/jamie-fenn-XhzdJk1za2k-unsplash.jpg) | ![Reference](./test-resources/originals/ravi-sharma-f2h77fiLu18-unsplash.jpg) | ![HM](./test-resources/processed/jamie-fenn-XhzdJk1za2k--ravi-sharma-f2h77fiLu18--histogram-matching.jpeg) | ![ET](./test-resources/processed/jamie-fenn-XhzdJk1za2k--ravi-sharma-f2h77fiLu18--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/jamie-fenn-XhzdJk1za2k-unsplash.jpg) | ![Reference](./test-resources/originals/tania-miron-EKX3Lx-t5CM-unsplash.jpg) | ![HM](./test-resources/processed/jamie-fenn-XhzdJk1za2k--tania-miron-EKX3Lx-t5CM--histogram-matching.jpeg) | ![ET](./test-resources/processed/jamie-fenn-XhzdJk1za2k--tania-miron-EKX3Lx-t5CM--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/pawel-czerwinski-6lQDFGOB1iw-unsplash.jpg) | ![Reference](./test-resources/originals/ales-krivec-4miBe6zg5r0-unsplash.jpg) | ![HM](./test-resources/processed/pawel-czerwinski-6lQDFGOB1iw--ales-krivec-4miBe6zg5r0--histogram-matching.jpeg) | ![ET](./test-resources/processed/pawel-czerwinski-6lQDFGOB1iw--ales-krivec-4miBe6zg5r0--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/pawel-czerwinski-6lQDFGOB1iw-unsplash.jpg) | ![Reference](./test-resources/originals/benjamin-voros-phIFdC6lA4E-unsplash.jpg) | ![HM](./test-resources/processed/pawel-czerwinski-6lQDFGOB1iw--benjamin-voros-phIFdC6lA4E--histogram-matching.jpeg) | ![ET](./test-resources/processed/pawel-czerwinski-6lQDFGOB1iw--benjamin-voros-phIFdC6lA4E--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/pawel-czerwinski-6lQDFGOB1iw-unsplash.jpg) | ![Reference](./test-resources/originals/jamie-fenn-XhzdJk1za2k-unsplash.jpg) | ![HM](./test-resources/processed/pawel-czerwinski-6lQDFGOB1iw--jamie-fenn-XhzdJk1za2k--histogram-matching.jpeg) | ![ET](./test-resources/processed/pawel-czerwinski-6lQDFGOB1iw--jamie-fenn-XhzdJk1za2k--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/pawel-czerwinski-6lQDFGOB1iw-unsplash.jpg) | ![Reference](./test-resources/originals/pine-watt-2Hzmz15wGik-unsplash.jpg) | ![HM](./test-resources/processed/pawel-czerwinski-6lQDFGOB1iw--pine-watt-2Hzmz15wGik--histogram-matching.jpeg) | ![ET](./test-resources/processed/pawel-czerwinski-6lQDFGOB1iw--pine-watt-2Hzmz15wGik--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/pawel-czerwinski-6lQDFGOB1iw-unsplash.jpg) | ![Reference](./test-resources/originals/ravi-sharma-f2h77fiLu18-unsplash.jpg) | ![HM](./test-resources/processed/pawel-czerwinski-6lQDFGOB1iw--ravi-sharma-f2h77fiLu18--histogram-matching.jpeg) | ![ET](./test-resources/processed/pawel-czerwinski-6lQDFGOB1iw--ravi-sharma-f2h77fiLu18--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/pawel-czerwinski-6lQDFGOB1iw-unsplash.jpg) | ![Reference](./test-resources/originals/tania-miron-EKX3Lx-t5CM-unsplash.jpg) | ![HM](./test-resources/processed/pawel-czerwinski-6lQDFGOB1iw--tania-miron-EKX3Lx-t5CM--histogram-matching.jpeg) | ![ET](./test-resources/processed/pawel-czerwinski-6lQDFGOB1iw--tania-miron-EKX3Lx-t5CM--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/pine-watt-2Hzmz15wGik-unsplash.jpg) | ![Reference](./test-resources/originals/ales-krivec-4miBe6zg5r0-unsplash.jpg) | ![HM](./test-resources/processed/pine-watt-2Hzmz15wGik--ales-krivec-4miBe6zg5r0--histogram-matching.jpeg) | ![ET](./test-resources/processed/pine-watt-2Hzmz15wGik--ales-krivec-4miBe6zg5r0--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/pine-watt-2Hzmz15wGik-unsplash.jpg) | ![Reference](./test-resources/originals/benjamin-voros-phIFdC6lA4E-unsplash.jpg) | ![HM](./test-resources/processed/pine-watt-2Hzmz15wGik--benjamin-voros-phIFdC6lA4E--histogram-matching.jpeg) | ![ET](./test-resources/processed/pine-watt-2Hzmz15wGik--benjamin-voros-phIFdC6lA4E--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/pine-watt-2Hzmz15wGik-unsplash.jpg) | ![Reference](./test-resources/originals/jamie-fenn-XhzdJk1za2k-unsplash.jpg) | ![HM](./test-resources/processed/pine-watt-2Hzmz15wGik--jamie-fenn-XhzdJk1za2k--histogram-matching.jpeg) | ![ET](./test-resources/processed/pine-watt-2Hzmz15wGik--jamie-fenn-XhzdJk1za2k--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/pine-watt-2Hzmz15wGik-unsplash.jpg) | ![Reference](./test-resources/originals/pawel-czerwinski-6lQDFGOB1iw-unsplash.jpg) | ![HM](./test-resources/processed/pine-watt-2Hzmz15wGik--pawel-czerwinski-6lQDFGOB1iw--histogram-matching.jpeg) | ![ET](./test-resources/processed/pine-watt-2Hzmz15wGik--pawel-czerwinski-6lQDFGOB1iw--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/pine-watt-2Hzmz15wGik-unsplash.jpg) | ![Reference](./test-resources/originals/ravi-sharma-f2h77fiLu18-unsplash.jpg) | ![HM](./test-resources/processed/pine-watt-2Hzmz15wGik--ravi-sharma-f2h77fiLu18--histogram-matching.jpeg) | ![ET](./test-resources/processed/pine-watt-2Hzmz15wGik--ravi-sharma-f2h77fiLu18--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/pine-watt-2Hzmz15wGik-unsplash.jpg) | ![Reference](./test-resources/originals/tania-miron-EKX3Lx-t5CM-unsplash.jpg) | ![HM](./test-resources/processed/pine-watt-2Hzmz15wGik--tania-miron-EKX3Lx-t5CM--histogram-matching.jpeg) | ![ET](./test-resources/processed/pine-watt-2Hzmz15wGik--tania-miron-EKX3Lx-t5CM--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/ravi-sharma-f2h77fiLu18-unsplash.jpg) | ![Reference](./test-resources/originals/ales-krivec-4miBe6zg5r0-unsplash.jpg) | ![HM](./test-resources/processed/ravi-sharma-f2h77fiLu18--ales-krivec-4miBe6zg5r0--histogram-matching.jpeg) | ![ET](./test-resources/processed/ravi-sharma-f2h77fiLu18--ales-krivec-4miBe6zg5r0--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/ravi-sharma-f2h77fiLu18-unsplash.jpg) | ![Reference](./test-resources/originals/benjamin-voros-phIFdC6lA4E-unsplash.jpg) | ![HM](./test-resources/processed/ravi-sharma-f2h77fiLu18--benjamin-voros-phIFdC6lA4E--histogram-matching.jpeg) | ![ET](./test-resources/processed/ravi-sharma-f2h77fiLu18--benjamin-voros-phIFdC6lA4E--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/ravi-sharma-f2h77fiLu18-unsplash.jpg) | ![Reference](./test-resources/originals/jamie-fenn-XhzdJk1za2k-unsplash.jpg) | ![HM](./test-resources/processed/ravi-sharma-f2h77fiLu18--jamie-fenn-XhzdJk1za2k--histogram-matching.jpeg) | ![ET](./test-resources/processed/ravi-sharma-f2h77fiLu18--jamie-fenn-XhzdJk1za2k--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/ravi-sharma-f2h77fiLu18-unsplash.jpg) | ![Reference](./test-resources/originals/pawel-czerwinski-6lQDFGOB1iw-unsplash.jpg) | ![HM](./test-resources/processed/ravi-sharma-f2h77fiLu18--pawel-czerwinski-6lQDFGOB1iw--histogram-matching.jpeg) | ![ET](./test-resources/processed/ravi-sharma-f2h77fiLu18--pawel-czerwinski-6lQDFGOB1iw--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/ravi-sharma-f2h77fiLu18-unsplash.jpg) | ![Reference](./test-resources/originals/pine-watt-2Hzmz15wGik-unsplash.jpg) | ![HM](./test-resources/processed/ravi-sharma-f2h77fiLu18--pine-watt-2Hzmz15wGik--histogram-matching.jpeg) | ![ET](./test-resources/processed/ravi-sharma-f2h77fiLu18--pine-watt-2Hzmz15wGik--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/ravi-sharma-f2h77fiLu18-unsplash.jpg) | ![Reference](./test-resources/originals/tania-miron-EKX3Lx-t5CM-unsplash.jpg) | ![HM](./test-resources/processed/ravi-f2h77fiLu18-sharma--tania-miron-EKX3Lx-t5CM--histogram-matching.jpeg) | ![ET](./test-resources/processed/ravi-f2h77fiLu18-sharma--tania-miron-EKX3Lx-t5CM--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/tania-miron-EKX3Lx-t5CM-unsplash.jpg) | ![Reference](./test-resources/originals/ales-krivec-4miBe6zg5r0-unsplash.jpg) | ![HM](./test-resources/processed/tania-miron-EKX3Lx-t5CM--ales-krivec-4miBe6zg5r0--histogram-matching.jpeg) | ![ET](./test-resources/processed/tania-miron-EKX3Lx-t5CM--ales-krivec-4miBe6zg5r0--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/tania-miron-EKX3Lx-t5CM-unsplash.jpg) | ![Reference](./test-resources/originals/benjamin-voros-phIFdC6lA4E-unsplash.jpg) | ![HM](./test-resources/processed/tania-miron-EKX3Lx-t5CM--benjamin-voros-phIFdC6lA4E--histogram-matching.jpeg) | ![ET](./test-resources/processed/tania-miron-EKX3Lx-t5CM--benjamin-voros-phIFdC6lA4E--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/tania-miron-EKX3Lx-t5CM-unsplash.jpg) | ![Reference](./test-resources/originals/jamie-fenn-XhzdJk1za2k-unsplash.jpg) | ![HM](./test-resources/processed/tania-miron-EKX3Lx-t5CM--jamie-fenn-XhzdJk1za2k--histogram-matching.jpeg) | ![ET](./test-resources/processed/tania-miron-EKX3Lx-t5CM--jamie-fenn-XhzdJk1za2k--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/tania-miron-EKX3Lx-t5CM-unsplash.jpg) | ![Reference](./test-resources/originals/pawel-czerwinski-6lQDFGOB1iw-unsplash.jpg) | ![HM](./test-resources/processed/tania-miron-EKX3Lx-t5CM--pawel-czerwinski-6lQDFGOB1iw--histogram-matching.jpeg) | ![ET](./test-resources/processed/tania-miron-EKX3Lx-t5CM--pawel-czerwinski-6lQDFGOB1iw--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/tania-miron-EKX3Lx-t5CM-unsplash.jpg) | ![Reference](./test-resources/originals/pine-watt-2Hzmz15wGik-unsplash.jpg) | ![HM](./test-resources/processed/tania-miron-EKX3Lx-t5CM--pine-watt-2Hzmz15wGik--histogram-matching.jpeg) | ![ET](./test-resources/processed/tania-miron-EKX3Lx-t5CM--pine-watt-2Hzmz15wGik--ellipsoid-transformation.jpeg)
| ![Input](./test-resources/originals/tania-miron-EKX3Lx-t5CM-unsplash.jpg) | ![Reference](./test-resources/originals/ravi-sharma-f2h77fiLu18-unsplash.jpg) | ![HM](./test-resources/processed/tania-miron-EKX3Lx-t5CM--ravi-sharma-f2h77fiLu18--histogram-matching.jpeg) | ![ET](./test-resources/processed/tania-miron-EKX3Lx-t5CM--ravi-sharma-f2h77fiLu18--ellipsoid-transformation.jpeg)
