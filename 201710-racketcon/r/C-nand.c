static void nand(wireidx_t aA, uint8_t aB,
                 wireidx_t bA, uint8_t bB,
                 wireidx_t oA, uint8_t oB) {
	uint8_t L = (WIRES[aA] & ((wire_t)1<<aB)) ? 1 : 0;
	uint8_t R = (WIRES[bA] & ((wire_t)1<<bB)) ? 1 : 0;
	uint8_t A = L & R ? 0 : 1;
	wire_t O = WIRES[oA];
	wire_t M = ((wire_t)1<<oB);
	WIRES[oA] = A ? (O | M) : (O & (~M));
}
