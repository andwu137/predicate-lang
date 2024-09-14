def lt(hand, n, card):
	return hand.count(card) < n
def eq(hand, n, card):
	return hand.count(card) == n
def gt(hand, n, card):
	return hand.count(card) > n
def has(hand, card):
	return card in hand
def if_(hand, b, f, g):
	return f if b else g
def main(hand):
	return ((((not func1) or func2)) and (func1 and lt(hand,2,4)))
def func1(hand):
	return (eq(hand,1,1) or gt(hand,2,2))
def func2(hand):
	return (not lt(hand,2,3))