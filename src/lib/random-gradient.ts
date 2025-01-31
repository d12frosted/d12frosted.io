import colors from 'tailwindcss/colors'

export type Direction = 'br' | 'r' | 'tr' | 't' | 'tl' | 'l' | 'bl' | 'b'

function getRandomItem<A>(arr: A[]): A {
  return arr[Math.floor(Math.random() * arr.length)]
}

function isNotAllowed(color: string): boolean {
  if (!color.match(/^[a-z]+$/)) return true
  const notAllowed = ['black', 'white', 'inherit', 'current', 'transparent']
  return notAllowed.includes(color)
}

export function getRandomColor(prevColor = ''): string {
  const allColors = Object.keys(colors)
  const color = getRandomItem(allColors)
  return isNotAllowed(color) || prevColor === color ? getRandomColor(prevColor) : color
}

export function getGradient(
  firstColor: string,
  secondColor: string,
  intensity = 300,
  direction: Direction = 'br'
): string {
  return `bg-gradient-to-${direction} from-${firstColor}-${intensity} to-${secondColor}-${intensity}`
}

export function getRandomGradient(intensity = 300, direction: Direction = 'br'): string {
  const c1 = getRandomColor()
  const c2 = getRandomColor(c1)
  return getGradient(c1, c2, intensity, direction)
}
