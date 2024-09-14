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
deck = {1:2,2:1,3:3,4:1}
def main(hand):
	return (((func1(hand) and lt(hand,2,4)) and (not (((not func1(hand)) or func2(hand))))) and func5(hand,func3,func4)(hand,func4,(not (((not func1(hand)) or func2(hand)))),3))
def func1(hand):
	return (eq(hand,1,1) or gt(hand,2,2))
def func2(hand):
	return (not lt(hand,2,3))
def func3(hand,f,b,c):
	return f(hand,(not b),2,c)
def func4(hand,b,n,c):
	return ((not b) or lt(hand,n,c))
def func5(hand,f,g):
	return if_(hand,lt(hand,2,1),f,g)