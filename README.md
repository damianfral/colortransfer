# colortransfer

`colortransfer` is a small tool that transfers the color distribution of one
image to another image. The final image will have the mean and standard
deviation of its color channels adjusted to match the mean and standard
deviation of the color channels of the source image. It's a clone of
<https://github.com/jrosebr1/color_transfer> but it works on the YCbCr color
space, and it is written in Haskell.

## Run

```shell
nix run github:damianfral/colortransfer -- \
  --source nice-colors.jpg \
  --target my-photo.jpg \
  --output my-photo-with-nice-colors.jpg
```

## Examples

![test1](images/autumn-fallingwater.png?raw=true)
![test2](images/oceansunset-oceanday.png?raw=true)
![test3](images/woods-storm.png?raw=true)
