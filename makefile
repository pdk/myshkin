IDOL = idol -t
ITRAN = icont -s -c -Sr300 -SF30 -Si1000
ILINK = icont -Sr800 -SF100 -Sg1000 -Sn3000 -Si2000 -SC40000


myshkin.icn : myshkin.iol
	$(IDOL) myshkin.iol

misc.icn : misc.iol
	$(IDOL) misc.iol

module.icn : module.iol
	$(IDOL) module.iol

parser.icn : parser.iol
	$(IDOL) parser.iol

translator.icn : translator.iol
	$(IDOL) translator.iol

evaluator.icn : evaluator.iol
	$(IDOL) evaluator.iol

file.icn : file.iol
	$(IDOL) file.iol


myshkin.u1 : myshkin.icn
	$(ITRAN) myshkin.icn

misc.u1 : misc.icn
	$(ITRAN) misc.icn

parser.u1 : parser.icn
	$(ITRAN) parser.icn

module.u1 : module.icn
	$(ITRAN) module.icn

translator.u1 : translator.icn
	$(ITRAN) translator.icn

evaluator.u1 : evaluator.icn
	$(ITRAN) evaluator.icn

file.u1 : file.icn
	$(ITRAN) file.icn


p : myshkin.u1 misc.u1 parser.u1 translator.u1 module.u1 evaluator.u1 file.u1
	updateIdolcode
	$(ILINK) myshkin.u1 misc.u1 parser.u1 translator.u1 module.u1 evaluator.u1 file.u1
