# Maintainer: Virgile Robles <virgile.robles@pm.me>
pkgname=ur-git
pkgver=r111.83e8ae7
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
  opam init --bare --no-setup
  if [[ -d "_opam" ]]; then
      opam install -y --deps-only .
  else
      opam switch create -y --deps-only .
  fi
  opam exec -- dune build @install
}

package() {
  cd "$srcdir/$pkgname"
  opam exec -- dune install --prefix="$pkgdir/usr/"
}
