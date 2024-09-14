def main(deck):
	return((func1(deck) and func2(deck)) and (func1(deck) and lt(2,deck[4])))
def func1(deck):
	return(has(3,deck[1]) or has(1,deck[2]))
def func2(deck):
	return(not has(3,deck[3]))