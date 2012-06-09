from coas import AssemblerAnnotation

def mapping(words):
    return ''.join(["%s\t%s\n" % (k, v) for k, v in words.items()])

def binaryOutput(data):
    return ''.join([struct.pack(">H", o) for o in data if not isinstance(o, AssemblerAnnotation)])

def intelHex(data):
    data = ''.join([struct.pack(">H", o) for o in bin if not isinstance(o, AssemblerAnnotation)])

    chunkSize = 32
    def chunk(c):
        for i in range(0,len(c),chunkSize):
            yield c[i:i+chunkSize]

    def format(address, rec_type, data = []):
        data = [len(data), address>>8, address & 0xFF, rec_type] + data
        data += [((sum(data) ^ 0xFF) + 1) & 0xFF]
        return ":" + (''.join(["%2X" % d for d in data])).replace(' ','0')

    for addr, block in enumerate(chunk([ord(c) for c in data])):
        yield format(addr * chunkSize, 0, block)
    yield format(0,1)

def datOutput(data):
    leadIn = False
    for k in data:
        if isinstance(k, AssemblerAnnotation):
            yield "; %s\n" % k.pos[3]
            leadIn = False
            continue 

        if not leadIn:
            yield "DAT "
            leadIn = True

        yield ("0x%4x" % k).replace(" ","0") + " "

def verilog(data):
    for k in data:
        if isinstance(k, AssemblerAnnotation):
            yield "// %s\n" % k.pos[3]
        else:
            yield ("%4x" % k).replace(" ","0") + " "

