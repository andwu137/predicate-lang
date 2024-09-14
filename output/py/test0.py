def lt(hand, n, card):
	return hand.count(card) < n
def eq(hand, n, card):
	return hand.count(card) == n
def gt(hand, n, card):
	return hand.count(card) > n
def has(hand, card):
	return card in hand
def main(hand):
	return(((not func1(hand)) or func2(hand)) and (func1(hand) and lt(hand,2,4)))
def func1(hand):
	return(eq(hand,1,1) or gt(hand,2,2))
def func2(hand):
	return(not lt(hand,2,3))