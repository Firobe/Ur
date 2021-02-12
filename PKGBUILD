# Maintainer: Virgile Robles <virgile.robles@pm.me>
pkgname=ur-git
pkgver=0.2
pkgrel=1
pkgdesc="Graphical implementation of the Royal Game of Ur"
arch=('x86_64')
url="https://github.com/Firobe/Ur"
license=('GPL')
groups=()
depends=('libgl' 'libffi' 'sdl2' 'sdl2_image' 'sdl2_mixer' 'sdl2_ttf')
makedepends=('opam')
optdepends=()
provides=()
conflicts=()
replaces=()
backup=()
options=()
install=
changelog=
source=("$pkgname::git+https://github.com/Firobe/Ur.git")
noextract=()
md5sums=('SKIP') #autofill using updpkgsums

pkgver() {
  cd "$pkgname"
  printf "r%s.%s" "$(git rev-list --count HEAD)" "$(git rev-parse --short HEAD)"
}

build() {
  cd "$srcdir/$pkgname"
  ./gen-dune.sh
  opam switch create -y --deps-only .
  dune build @install
}

package() {
  cd "$srcdir/$pkgname"
  dune install --prefix="$pkgdir/usr/"
}
