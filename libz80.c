#include "libz80.h"
#include <stdio.h>

#define bool int
#define true 1
#define false 0

// Utils

uint8_t FETCH(Z80 *cpu, uint16_t address) {
    return cpu->memory[address];
}

uint16_t FETCH16(Z80 *cpu, uint16_t address) {
    return (cpu->memory[address + 1] << 8) | cpu->memory[address];
}

void WRITE(Z80 *cpu, uint16_t address, uint8_t value) {
    if (address <= ROM_END) {
        cpu->memory[address] = value;
    }
}

void WRITE16(Z80 *cpu, uint16_t address, uint16_t value) {
    if (address <= ROM_END) {
        cpu->memory[address] = value & 0xff;
        cpu->memory[address + 1] = (value & 0xff00) >> 8;
    }
}

void set_RP_to_WORD(Z80 *cpu, uint8_t rp, uint16_t value) {
    switch (rp) {
        case 0:
            cpu->B = (value & 0xff00) >> 8;
            cpu->C =  value & 0xff;
            break;
        case 1:
            cpu->D = (value & 0xff00) >> 8;
            cpu->E =  value & 0xff;
            break;
        case 2:
            cpu->H = (value & 0xff00) >> 8;
            cpu->L =  value & 0xff;
            break;
        case 3:
            SP = value;
            break;
    }
}

uint16_t get_RP_value(Z80 *cpu, uint8_t rp) {
    switch (rp) {
        case 0:
            return BC;
        case 1:
            return DE;
        case 2:
            return HL;
        case 3:
            return SP;
    }
}

void set_r_to_BYTE(Z80 *cpu, uint8_t r, uint8_t value) {
    switch (r) {
        case 0:
            cpu->B = value;
            break;
        case 1:
            cpu->C = value;
            break;
        case 2:
            cpu->D = value;
            break;
        case 3:
            cpu->E = value;
            break;
        case 4:
            cpu->H = value;
            break;
        case 5:
            cpu->L = value;
            break;
        case 6:
            // Should be memory
            printf("Warning treating mem as reg\n");
            exit(0);
        case 7:
            cpu->A = value;
            break;
    }
}

uint8_t get_r_value(Z80 *cpu, uint8_t r) {
    switch (r) {
        case 0:
            return cpu->B;
        case 1:
            return cpu->C;
        case 2:
            return cpu->D;
        case 3:
            return cpu->E;
        case 4:
            return cpu->H;
        case 5:
            return cpu->L;
        case 6:
            // Should be memory
            printf("Warning treating mem as reg\n");
            exit(0);
        case 7:
            return cpu->A;
    }
}

void sign_flag(Z80 *cpu, uint8_t value) {
    cpu->F = (cpu->F & 0x7f) | (value & 0x80);
    // Keep bit 6 to 0 from flag, keep bit 7 of value then merge
}

void zero_flag(Z80 *cpu, uint8_t value) {
    if (value == 0) cpu->F |= ZF;
    else cpu->F &= ~(ZF);
}

void f5_flag(Z80 *cpu, uint8_t value) {
    cpu->F = (cpu->F & ~YF) | (value & YF);
}

void half_carry_flag(Z80 *cpu, uint8_t a, uint8_t b, int sub) {
    // No fun around here >:(
    if (sub) {
        if ((a & 0xF) < (b & 0xF)) cpu->F |= HF;
        else cpu->F &= ~(HF);
    } else {
        if (((a & 0xF) + (b & 0xF)) > 0xF) cpu->F |= HF;
        else cpu->F &= ~(HF);
    }
}

void f3_flag(Z80 *cpu, uint8_t value) {
    cpu->F = (cpu->F & ~XF) | (value & XF);
}

void overflow_flag(Z80 *cpu, uint8_t a, uint8_t b, int sub) {
    int8_t sa = (int8_t)a;
    int8_t sb = sub ? -(int8_t)b : (int8_t)b;
    int8_t res = sa + sb;

    // Overflow occurs if the sign of 'a' and 'b' are the same,
    // but the result has a different sign.
    if (((sa ^ res) & (sb ^ res)) < 0)
        cpu->F |= PF;
    else
        cpu->F &= ~PF;
}

void parity_overflow_flag(Z80 *cpu, uint8_t value) {
    if (parity_table[value]) cpu->F |= PF;
    else cpu->F &= ~(PF);
}

void subtract_flag(Z80 *cpu, int sub) {
    if (sub) cpu->F |= NF;
    else cpu->F &= ~(NF);
}

void carry_flag(Z80 *cpu, uint8_t value) {
}

uint8_t left_rotate(uint8_t n) {
    return (n << 1) | (n >> 7);
}

uint8_t right_rotate(uint8_t n) {
    return (n >> 1) | (n << 7);
}

bool condition_met(Z80 *cpu, uint8_t cond_id) {
    switch (cond_id) {
        case 0: return !(cpu->F & ZF);
        case 1: return (cpu->F & ZF);
        case 2: return !(cpu->F & CF);
        case 3: return (cpu->F & CF);
        case 4: return !(cpu->F & PF);
        case 5: return (cpu->F & PF);
        case 6: return !(cpu->F & SF);
        case 7: return (cpu->F & SF);
    }
    return false;
}

// Instruction handler

// Main instructions

void nop(Z80 *cpu) {
    PC++;
    CYCLES = 4;
}

void ld_SS_WORD(Z80 *cpu) {
    uint8_t rp = (FETCH(cpu, PC) & 0x30) >> 4;
    set_RP_to_WORD(cpu, rp, FETCH16(cpu, ++PC));    // Fetch word at next address and stores it into rp
    PC += 2;    // 3 bytes total
    CYCLES = 10;
}

void ld_vbc_a(Z80 *cpu) {
    WRITE(cpu, BC, cpu->A);
    PC += 1;
    CYCLES = 7;
}

void inc_SS(Z80 *cpu) {
    uint8_t rp = (FETCH(cpu, PC) & 0x30) >> 4;
    set_RP_to_WORD(cpu, rp, get_RP_value(cpu, rp) + 1); // Fetch word at next address and stores it into rp
    PC += 1;
    CYCLES = 6;
}

void V_J(Z80 *cpu) {
    uint8_t r = (FETCH(cpu, PC) & 0x38) >> 3;
    int8_t operation = FETCH(cpu, PC) & 1 ? -1: 1;
    uint16_t res = get_r_value(cpu, r) + operation;
    subtract_flag(cpu, FETCH(cpu, PC) & 1);
    overflow_flag(cpu, res, 1, operation);
    half_carry_flag(cpu, get_r_value(cpu, r), 1, FETCH(cpu, PC) & 1);
    zero_flag(cpu, res);
    sign_flag(cpu, res);
    f5_flag(cpu, res);
    f3_flag(cpu, res);
    set_r_to_BYTE(cpu, r, (uint8_t)res);    // Set to result after checking flags
    CYCLES = 4;
    PC++;
}

void ld_J_BYTE(Z80 *cpu) {
    uint8_t r = (FETCH(cpu, PC) & 0x38) >> 3;
    PC++;
    set_r_to_BYTE(cpu, r, FETCH(cpu, PC));
    PC++;
    CYCLES = 7;
}

void rlca(Z80 *cpu) {
    uint8_t res = (cpu->A << 1) | ((cpu->A & 0x80) >> 7); // rotate left, bit 7 to bit 0
    cpu->F = (cpu->F & ~(HF | NF | CF)) | ((cpu->A & 0x80) >> 7); // set carry, clear N/H
    cpu->A = res;
    PC++;
    CYCLES = 4;
}

void ex_af_af_(Z80 *cpu) {
    uint8_t t_A = cpu->A;
    uint8_t t_F = cpu->F;
    cpu->A = cpu->A_;
    cpu->F = cpu->F_;
    cpu->A_ = t_A;
    cpu->F_ = t_F;
    PC++;
    CYCLES = 4;
}

void add_hl_SS(Z80 *cpu) {
    uint8_t rp = (FETCH(cpu, PC) & 0x30) >> 4;
    uint32_t res = HL + get_RP_value(cpu, rp);
    if (res > 0xffff) cpu->F |= CF;
    else cpu->F &= ~CF;
    half_carry_flag(cpu, HL, get_RP_value(cpu, rp) >> 8, 0);    // 0 = no sub
    subtract_flag(cpu, 0);
    f3_flag(cpu, res >> 8);
    f5_flag(cpu, res >> 8);     // F3 and F5 from upper byte
    set_RP_to_WORD(cpu, 2, res); // 2 = HL register encoding
    PC++;
    CYCLES = 11;
}

void ld_a_vbc(Z80 *cpu) {
    cpu->A = FETCH(cpu, BC);
    PC++;
    CYCLES = 7;
}

void dec_SS(Z80 *cpu) {
    uint8_t rp = (FETCH(cpu, PC) & 0x30) >> 4;
    set_RP_to_WORD(cpu, rp, get_RP_value(cpu, rp) - 1); // Fetch word at next address and stores it into rp
    PC += 1;
    CYCLES = 6;
}

void rrca(Z80 *cpu) {
    uint8_t res = (cpu->A >> 1) | ((cpu->A & 0x80) << 7); // rotate right, bit 7 to bit 0
    cpu->F = (cpu->F & ~(HF | NF | CF)) | (cpu->A & 0x01); // set carry, clear N/H
    cpu->A = res;
    PC++;
    CYCLES = 4;
}

void djnz_OFFSET(Z80 *cpu) {
    cpu->B--;
    if (cpu->B != 0) { PC += (int8_t)FETCH(cpu, PC + 1) + 2; CYCLES = 13; }
    else { PC += 2; CYCLES = 8; }
}

void ld_vde_a(Z80 *cpu) {
    WRITE(cpu, DE, cpu->A);
    PC++;
    CYCLES = 7;
}

void rla(Z80 *cpu) {
    uint8_t res = (cpu->A << 1) | (cpu->F & CF); // rotate left
    cpu->F = (cpu->F & ~(HF | NF | CF)) | ((cpu->A & 0x80) >> 7); // set carry, clear N/H
    cpu->A = res;
    PC++;
    CYCLES = 4;
}

void jr_OFFSET(Z80 *cpu) {
    int8_t offset = (int8_t)FETCH(cpu, PC + 1);
    PC += offset + 2;
    CYCLES = 12;
}

void ld_a_vde(Z80 *cpu) {
    cpu->A = FETCH(cpu, DE);
    PC++;
    CYCLES = 7;
}

void rra(Z80 *cpu) {
    uint8_t res = (cpu->A >> 1) | ((cpu->F & CF) << 7); // rotate right with carry
    cpu->F = (cpu->F & ~(HF | NF | CF)) | (cpu->A & 0x01); // set carry, clear N/H
    cpu->A = res;
    PC++;
    CYCLES = 4;
}

void ld_vWORD_hl(Z80 *cpu) {
    WRITE16(cpu, FETCH16(cpu, PC + 1), HL);
    PC += 3;
    CYCLES = 16;
}

void daa(Z80 *cpu) {
    uint8_t correction = 0;
    uint8_t C = cpu->F & 0x01;  // Carry flag
    uint8_t H = cpu->F & 0x10;  // Half-carry flag
    uint8_t N = cpu->F & 0x02;  // Subtract flag

    if (!N) {
        if ((cpu->A & 0x0F) > 9 || H) correction |= 0x06;
        if (cpu->A > 0x99 || C)     correction |= 0x60;
        cpu->A += correction;
    } else {
        if (H) correction |= 0x06;
        if (C) correction |= 0x60;
        cpu->A -= correction;
    }

    // Update flags
    cpu->F &= 0x02; // Keep N flag only
    if (cpu->A == 0) cpu->F |= 0x80;         // Z (Zero)
    if (correction & 0x60) cpu->F |= 0x01;  // C (Carry)
    if (((cpu->A ^ correction) & 0x10)) cpu->F |= 0x10; // H (Half-carry) if needed
    PC ++;
    CYCLES = 4;
}

void jr_Z_OFFSET(Z80 *cpu) {
    uint8_t cc_id = (FETCH(cpu, PC) >> 3) & 0x03;
    if (condition_met(cpu, cc_id)) {
        int8_t offset = (int8_t)FETCH(cpu, PC + 1);
        PC += offset + 2;
        CYCLES = 12;
    } else {
        PC += 2;
        CYCLES = 7;
    }
}

void ld_hl_vWORD(Z80 *cpu) {
    set_RP_to_WORD(cpu, 2, FETCH16(cpu, FETCH16(cpu, PC + 1))); // 2 = HL
    PC += 3;
    CYCLES = 16;
}

void cpl(Z80 *cpu) {
    cpu->A = ~cpu->A;
    cpu->F |= (NF | HF);
    PC++;
    CYCLES = 4;
}

void ld_vWORD_a(Z80 *cpu) {
    cpu->A = FETCH(cpu, FETCH16(cpu, PC + 1));
    PC += 3;
    CYCLES = 13;
}

void V_vhl(Z80 *cpu) {
    int8_t operation = FETCH(cpu, PC) & 1 ? -1: 1;
    uint8_t val = FETCH(cpu, HL);
    uint16_t res = val + operation;
    subtract_flag(cpu, FETCH(cpu, PC) & 1);
    overflow_flag(cpu, val, 1, operation);
    half_carry_flag(cpu, val, 1, FETCH(cpu, PC) & 1);
    zero_flag(cpu, res);
    sign_flag(cpu, res);
    f5_flag(cpu, res);
    f3_flag(cpu, res);
    WRITE(cpu, HL, (uint8_t)res);    // Set to result after checking flags
    CYCLES = 11;
    PC++;
}

void ld_vhl_BYTE(Z80 *cpu) {
}

void scf(Z80 *cpu) {
}

void ld_a_vWORD(Z80 *cpu) {
}

void ccf(Z80 *cpu) {
}

void ld_J_K(Z80 *cpu) {
}

void ld_vhl_K(Z80 *cpu) {
}

void hook(Z80 *cpu) {
}

void ld_J_vhl(Z80 *cpu) {
}

void halt(Z80 *cpu) {
}

void U_a_K(Z80 *cpu) {
}

void U_a_vhl(Z80 *cpu) {
}

void ret_Z(Z80 *cpu) {
}

void pop_TT(Z80 *cpu) {
}

void jp_Z_WORD(Z80 *cpu) {
}

void jp_WORD(Z80 *cpu) {
}

void call_Z_WORD(Z80 *cpu) {
}

void push_TT(Z80 *cpu) {
}

void U_a_BYTE(Z80 *cpu) {
}

void rst_N(Z80 *cpu) {
}

void ret(Z80 *cpu) {
}

void cb_prefix(Z80 *cpu) {
}

void call_WORD(Z80 *cpu) {
}

void out_vBYTE_a(Z80 *cpu) {
}

void exx(Z80 *cpu) {
}

void in_a_vBYTE(Z80 *cpu) {
}

void dd_prefix(Z80 *cpu) {
}

void ex_vsp_hl(Z80 *cpu) {
}

void jp_hl(Z80 *cpu) {
}

void ex_de_hl(Z80 *cpu) {
}

void ed_prefix(Z80 *cpu) {
}

void di(Z80 *cpu) {
}

void ld_sp_hl(Z80 *cpu) {
}

void ei(Z80 *cpu) {
}

void fd_prefix(Z80 *cpu) {
}

// CB prefix

void G_K(Z80 *cpu) {
}

void G_vhl(Z80 *cpu) {
}

void bit_N_K(Z80 *cpu) {
}

void bit_N_vhl(Z80 *cpu) {
}

void M_N_K(Z80 *cpu) {
}

void M_N_vhl(Z80 *cpu) {
}

// ED prefix

void ed_illegal(Z80 *cpu) {
}

void in_J_vc(Z80 *cpu) {
}

void out_vc_J(Z80 *cpu) {
}

void sbc_hl_SS(Z80 *cpu) {
}

void ld_vWORD_SS(Z80 *cpu) {
}

void neg(Z80 *cpu) {
}

void retn(Z80 *cpu) {
}

void im_0(Z80 *cpu) {
}

void ld_i_a(Z80 *cpu) {
}

void adc_hl_SS(Z80 *cpu) {
}

void ld_SS_vWORD(Z80 *cpu) {
}

void reti(Z80 *cpu) {
}

void ld_r_a(Z80 *cpu) {
}

void im_1(Z80 *cpu) {
}

void ld_a_i(Z80 *cpu) {
}

void reti_retn(Z80 *cpu) {
}

void im_2(Z80 *cpu) {
}

void ld_a_r(Z80 *cpu) {
}

void rrd(Z80 *cpu) {
}

void rld(Z80 *cpu) {
}

void in_vc(Z80 *cpu) {
}

void out_vc_0(Z80 *cpu) {
}

void ldi(Z80 *cpu) {
}

void cpi(Z80 *cpu) {
}

void ini(Z80 *cpu) {
}

void outi(Z80 *cpu) {
}

void ldd(Z80 *cpu) {
}

void cpd(Z80 *cpu) {
}

void ind(Z80 *cpu) {
}

void outd(Z80 *cpu) {
}

void ldir(Z80 *cpu) {
}

void cpir(Z80 *cpu) {
}

void inir(Z80 *cpu) {
}

void otir(Z80 *cpu) {
}

void lddr(Z80 *cpu) {
}

void cpdr(Z80 *cpu) {
}

void indr(Z80 *cpu) {
}

void otdr(Z80 *cpu) {
}

// XY

void nop_nop(Z80 *cpu) {
}

void xy_illegal(Z80 *cpu) {
}

void V_O(Z80 *cpu) {
}

void ld_O_BYTE(Z80 *cpu) {
}

void add_XY_WW(Z80 *cpu) {
}

void ld_XY_WORD(Z80 *cpu) {
}

void ld_vWORD_XY(Z80 *cpu) {
}

void inc_XY(Z80 *cpu) {
}

void ld_XY_vWORD(Z80 *cpu) {
}

void dec_XY(Z80 *cpu) {
}

void V_vXYpOFFSET(Z80 *cpu) {
}

void ld_vXYpOFFSET_BYTE(Z80 *cpu) {
}

void ld_O_P(Z80 *cpu) {
}

void ld_J_vXYpOFFSET(Z80 *cpu) {
}

void ld_vXYpOFFSET_K(Z80 *cpu) {
}

void U_a_P(Z80 *cpu) {
}

void U_a_vXYpOFFSET(Z80 *cpu) {
}

void xy_cb_prefix(Z80 *cpu) {
}

void xy_xy(Z80 *cpu) {
}

void pop_XY(Z80 *cpu) {
}

void ex_vsp_XY(Z80 *cpu) {
}

void push_XY(Z80 *cpu) {
}

void jp_XY(Z80 *cpu) {
}

void ld_sp_XY(Z80 *cpu) {
}

// XY CB

void G_vXYpOFFSET(Z80 *cpu) {
}

void G_vXYpOFFSET_K(Z80 *cpu) {
}

void bit_N_vXYpOFFSET(Z80 *cpu) {
}

void M_N_vXYpOFFSET(Z80 *cpu) {
}

void M_N_vXYpOFFSET_K(Z80 *cpu) {
}

// Initialize opcode dispatch table
typedef void (*Insn)(Z80 *cpu);

static Insn const insn_table[256] = {
/*	0	     1		 2	      3		   4		5	  6	       7	 8	      9		 A	      B		  C	       D	  E	     F */
/* 0 */ nop,	     ld_SS_WORD, ld_vbc_a,    inc_SS,	   V_J,		V_J,	  ld_J_BYTE,   rlca,	 ex_af_af_,   add_hl_SS, ld_a_vbc,    dec_SS,	  V_J,	       V_J,	  ld_J_BYTE, rrca,
/* 1 */ djnz_OFFSET, ld_SS_WORD, ld_vde_a,    inc_SS,	   V_J,		V_J,	  ld_J_BYTE,   rla,	 jr_OFFSET,   add_hl_SS, ld_a_vde,    dec_SS,	  V_J,	       V_J,	  ld_J_BYTE, rra,
/* 2 */ jr_Z_OFFSET, ld_SS_WORD, ld_vWORD_hl, inc_SS,	   V_J,		V_J,	  ld_J_BYTE,   daa,	 jr_Z_OFFSET, add_hl_SS, ld_hl_vWORD, dec_SS,	  V_J,	       V_J,	  ld_J_BYTE, cpl,
/* 3 */ jr_Z_OFFSET, ld_SS_WORD, ld_vWORD_a,  inc_SS,	   V_vhl,	V_vhl,	  ld_vhl_BYTE, scf,	 jr_Z_OFFSET, add_hl_SS, ld_a_vWORD,  dec_SS,	  V_J,	       V_J,	  ld_J_BYTE, ccf,
/* 4 */ nop,	     ld_J_K,	 ld_J_K,      ld_J_K,	   ld_J_K,	ld_J_K,	  ld_J_vhl,    ld_J_K,	 ld_J_K,      nop,	 ld_J_K,      ld_J_K,	  ld_J_K,      ld_J_K,	  ld_J_vhl,  ld_J_K,
/* 5 */ ld_J_K,	     ld_J_K,	 nop,	      ld_J_K,	   ld_J_K,	ld_J_K,	  ld_J_vhl,    ld_J_K,	 ld_J_K,      ld_J_K,	 ld_J_K,      nop,	  ld_J_K,      ld_J_K,	  ld_J_vhl,  ld_J_K,
/* 6 */ ld_J_K,	     ld_J_K,	 ld_J_K,      ld_J_K,	   hook,	ld_J_K,	  ld_J_vhl,    ld_J_K,	 ld_J_K,      ld_J_K,	 ld_J_K,      ld_J_K,	  ld_J_K,      nop,	  ld_J_vhl,  ld_J_K,
/* 7 */ ld_vhl_K,    ld_vhl_K,	 ld_vhl_K,    ld_vhl_K,	   ld_vhl_K,	ld_vhl_K, halt,	       ld_vhl_K, ld_J_K,      ld_J_K,	 ld_J_K,      ld_J_K,	  ld_J_K,      ld_J_K,	  ld_J_vhl,  nop,
/* 8 */ U_a_K,	     U_a_K,	 U_a_K,	      U_a_K,	   U_a_K,	U_a_K,	  U_a_vhl,     U_a_K,	 U_a_K,	      U_a_K,	 U_a_K,	      U_a_K,	  U_a_K,       U_a_K,	  U_a_vhl,   U_a_K,
/* 9 */ U_a_K,	     U_a_K,	 U_a_K,	      U_a_K,	   U_a_K,	U_a_K,	  U_a_vhl,     U_a_K,	 U_a_K,	      U_a_K,	 U_a_K,	      U_a_K,	  U_a_K,       U_a_K,	  U_a_vhl,   U_a_K,
/* A */ U_a_K,	     U_a_K,	 U_a_K,	      U_a_K,	   U_a_K,	U_a_K,	  U_a_vhl,     U_a_K,	 U_a_K,	      U_a_K,	 U_a_K,	      U_a_K,	  U_a_K,       U_a_K,	  U_a_vhl,   U_a_K,
/* B */ U_a_K,	     U_a_K,	 U_a_K,	      U_a_K,	   U_a_K,	U_a_K,	  U_a_vhl,     U_a_K,	 U_a_K,	      U_a_K,	 U_a_K,	      U_a_K,	  U_a_K,       U_a_K,	  U_a_vhl,   U_a_K,
/* C */ ret_Z,	     pop_TT,	 jp_Z_WORD,   jp_WORD,	   call_Z_WORD, push_TT,  U_a_BYTE,    rst_N,	 ret_Z,	      ret,	 jp_Z_WORD,   cb_prefix,  call_Z_WORD, call_WORD, U_a_BYTE,  rst_N,
/* D */ ret_Z,	     pop_TT,	 jp_Z_WORD,   out_vBYTE_a, call_Z_WORD, push_TT,  U_a_BYTE,    rst_N,	 ret_Z,	      exx,	 jp_Z_WORD,   in_a_vBYTE, call_Z_WORD, dd_prefix, U_a_BYTE,  rst_N,
/* E */ ret_Z,	     pop_TT,	 jp_Z_WORD,   ex_vsp_hl,   call_Z_WORD, push_TT,  U_a_BYTE,    rst_N,	 ret_Z,	      jp_hl,	 jp_Z_WORD,   ex_de_hl,	  call_Z_WORD, ed_prefix, U_a_BYTE,  rst_N,
/* F */ ret_Z,	     pop_TT,	 jp_Z_WORD,   di,	   call_Z_WORD, push_TT,  U_a_BYTE,    rst_N,	 ret_Z,	      ld_sp_hl,	 jp_Z_WORD,   ei,	  call_Z_WORD, fd_prefix, U_a_BYTE,  rst_N};

static Insn const cb_insn_table[256] = {
/*	0	 1	  2	   3	    4	     5	      6		 7	  8	   9	    A	     B	      C	       D	E	   F */
/* 0 */ G_K,	 G_K,	  G_K,	   G_K,	    G_K,     G_K,     G_vhl,	 G_K,	  G_K,	   G_K,	    G_K,     G_K,     G_K,     G_K,	G_vhl,	   G_K,
/* 1 */ G_K,	 G_K,	  G_K,	   G_K,	    G_K,     G_K,     G_vhl,	 G_K,	  G_K,	   G_K,	    G_K,     G_K,     G_K,     G_K,	G_vhl,	   G_K,
/* 2 */ G_K,	 G_K,	  G_K,	   G_K,	    G_K,     G_K,     G_vhl,	 G_K,	  G_K,	   G_K,	    G_K,     G_K,     G_K,     G_K,	G_vhl,	   G_K,
/* 3 */ G_K,	 G_K,	  G_K,	   G_K,	    G_K,     G_K,     G_vhl,	 G_K,	  G_K,	   G_K,	    G_K,     G_K,     G_K,     G_K,	G_vhl,	   G_K,
/* 4 */ bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_vhl, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_vhl, bit_N_K,
/* 5 */ bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_vhl, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_vhl, bit_N_K,
/* 6 */ bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_vhl, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_vhl, bit_N_K,
/* 7 */ bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_vhl, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_K, bit_N_vhl, bit_N_K,
/* 8 */ M_N_K,	 M_N_K,	  M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_vhl,	 M_N_K,	  M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_K,	M_N_vhl,   M_N_K,
/* 9 */ M_N_K,	 M_N_K,	  M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_vhl,	 M_N_K,	  M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_K,	M_N_vhl,   M_N_K,
/* A */ M_N_K,	 M_N_K,	  M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_vhl,	 M_N_K,	  M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_K,	M_N_vhl,   M_N_K,
/* B */ M_N_K,	 M_N_K,	  M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_vhl,	 M_N_K,	  M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_K,	M_N_vhl,   M_N_K,
/* C */ M_N_K,	 M_N_K,	  M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_vhl,	 M_N_K,	  M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_K,	M_N_vhl,   M_N_K,
/* D */ M_N_K,	 M_N_K,	  M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_vhl,	 M_N_K,	  M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_K,	M_N_vhl,   M_N_K,
/* E */ M_N_K,	 M_N_K,	  M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_vhl,	 M_N_K,	  M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_K,	M_N_vhl,   M_N_K,
/* F */ M_N_K,	 M_N_K,	  M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_vhl,	 M_N_K,	  M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_K,   M_N_K,	M_N_vhl,   M_N_K};

static Insn const ed_insn_table[256] = {
/*	0	    1		2	    3		 4	     5		 6	     7		 8	     9		 A	     B		  C	      D		  E	      F */
/* 0 */ ed_illegal, ed_illegal, ed_illegal, ed_illegal,	 ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal,  ed_illegal, ed_illegal, ed_illegal, ed_illegal,
/* 1 */ ed_illegal, ed_illegal, ed_illegal, ed_illegal,	 ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal,  ed_illegal, ed_illegal, ed_illegal, ed_illegal,
/* 2 */ ed_illegal, ed_illegal, ed_illegal, ed_illegal,	 ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal,  ed_illegal, ed_illegal, ed_illegal, ed_illegal,
/* 3 */ ed_illegal, ed_illegal, ed_illegal, ed_illegal,	 ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal,  ed_illegal, ed_illegal, ed_illegal, ed_illegal,
/* 4 */ in_J_vc,    out_vc_J,	sbc_hl_SS,  ld_vWORD_SS, neg,	     retn,	 im_0,	     ld_i_a,	 in_J_vc,    out_vc_J,	 adc_hl_SS,  ld_SS_vWORD, neg,	      reti,	  im_0,	      ld_r_a,
/* 5 */ in_J_vc,    out_vc_J,	sbc_hl_SS,  ld_vWORD_SS, neg,	     retn,	 im_1,	     ld_a_i,	 in_J_vc,    out_vc_J,	 adc_hl_SS,  ld_SS_vWORD, neg,	      reti_retn,  im_2,	      ld_a_r,
/* 6 */ in_J_vc,    out_vc_J,	sbc_hl_SS,  ld_vWORD_SS, neg,	     retn,	 im_0,	     rrd,	 in_J_vc,    out_vc_J,	 adc_hl_SS,  ld_SS_vWORD, neg,	      reti_retn,  im_0,	      rld,
/* 7 */ in_vc,	    out_vc_0,	sbc_hl_SS,  ld_vWORD_SS, neg,	     retn,	 im_1,	     ed_illegal, in_J_vc,    out_vc_J,	 adc_hl_SS,  ld_SS_vWORD, neg,	      reti_retn,  im_2,	      ed_illegal,
/* 8 */ ed_illegal, ed_illegal, ed_illegal, ed_illegal,	 ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal,  ed_illegal, ed_illegal, ed_illegal, ed_illegal,
/* 9 */ ed_illegal, ed_illegal, ed_illegal, ed_illegal,	 ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal,  ed_illegal, ed_illegal, ed_illegal, ed_illegal,
/* A */ ldi,	    cpi,	ini,	    outi,	 ed_illegal, ed_illegal, ed_illegal, ed_illegal, ldd,	     cpd,	 ind,	     outd,	  ed_illegal, ed_illegal, ed_illegal, ed_illegal,
/* B */ ldir,	    cpir,	inir,	    otir,	 ed_illegal, ed_illegal, ed_illegal, ed_illegal, lddr,	     cpdr,	 indr,	     otdr,	  ed_illegal, ed_illegal, ed_illegal, ed_illegal,
/* C */ ed_illegal, ed_illegal, ed_illegal, ed_illegal,	 ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal,  ed_illegal, ed_illegal, ed_illegal, ed_illegal,
/* D */ ed_illegal, ed_illegal, ed_illegal, ed_illegal,	 ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal,  ed_illegal, ed_illegal, ed_illegal, ed_illegal,
/* E */ ed_illegal, ed_illegal, ed_illegal, ed_illegal,	 ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal,  ed_illegal, ed_illegal, ed_illegal, ed_illegal,
/* F */ ed_illegal, ed_illegal, ed_illegal, ed_illegal,	 ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal, ed_illegal,  ed_illegal, ed_illegal, ed_illegal, ed_illegal};

static Insn const xy_insn_table[256] = {
/*	0		 1		  2		   3		    4		     5		      6			  7		   8	       9	   A		B	      C		  D	      E		       F */
/* 0 */ nop_nop,	 xy_illegal,	  xy_illegal,	   xy_illegal,	    V_O,	     V_O,	      ld_O_BYTE,	  xy_illegal,	   xy_illegal, add_XY_WW,  xy_illegal,	xy_illegal,   V_O,	  V_O,	      ld_O_BYTE,       xy_illegal,
/* 1 */ xy_illegal,	 xy_illegal,	  xy_illegal,	   xy_illegal,	    V_O,	     V_O,	      ld_O_BYTE,	  xy_illegal,	   xy_illegal, add_XY_WW,  xy_illegal,	xy_illegal,   V_O,	  V_O,	      ld_O_BYTE,       xy_illegal,
/* 2 */ xy_illegal,	 ld_XY_WORD,	  ld_vWORD_XY,	   inc_XY,	    V_O,	     V_O,	      ld_O_BYTE,	  xy_illegal,	   xy_illegal, add_XY_WW,  ld_XY_vWORD, dec_XY,	      V_O,	  V_O,	      ld_O_BYTE,       xy_illegal,
/* 3 */ xy_illegal,	 xy_illegal,	  xy_illegal,	   xy_illegal,	    V_vXYpOFFSET,    V_vXYpOFFSET,    ld_vXYpOFFSET_BYTE, xy_illegal,	   xy_illegal, add_XY_WW,  xy_illegal,	xy_illegal,   V_O,	  V_O,	      ld_O_BYTE,       xy_illegal,
/* 4 */ nop_nop,	 ld_O_P,	  ld_O_P,	   ld_O_P,	    ld_O_P,	     ld_O_P,	      ld_J_vXYpOFFSET,	  ld_O_P,	   ld_O_P,     nop_nop,	   ld_O_P,	ld_O_P,	      ld_O_P,	  ld_O_P,     ld_J_vXYpOFFSET, ld_O_P,
/* 5 */ ld_O_P,		 ld_O_P,	  nop_nop,	   ld_O_P,	    ld_O_P,	     ld_O_P,	      ld_J_vXYpOFFSET,	  ld_O_P,	   ld_O_P,     ld_O_P,	   ld_O_P,	nop_nop,      ld_O_P,	  ld_O_P,     ld_J_vXYpOFFSET, ld_O_P,
/* 6 */ ld_O_P,		 ld_O_P,	  ld_O_P,	   ld_O_P,	    nop_nop,	     ld_O_P,	      ld_J_vXYpOFFSET,	  ld_O_P,	   ld_O_P,     ld_O_P,	   ld_O_P,	ld_O_P,	      ld_O_P,	  nop_nop,    ld_J_vXYpOFFSET, ld_O_P,
/* 7 */ ld_vXYpOFFSET_K, ld_vXYpOFFSET_K, ld_vXYpOFFSET_K, ld_vXYpOFFSET_K, ld_vXYpOFFSET_K, ld_vXYpOFFSET_K, xy_illegal,	  ld_vXYpOFFSET_K, ld_O_P,     ld_O_P,	   ld_O_P,	ld_O_P,	      ld_O_P,	  ld_O_P,     ld_J_vXYpOFFSET, nop_nop,
/* 8 */ U_a_P,		 U_a_P,		  U_a_P,	   U_a_P,	    U_a_P,	     U_a_P,	      U_a_vXYpOFFSET,	  U_a_P,	   U_a_P,      U_a_P,	   U_a_P,	U_a_P,	      U_a_P,	  U_a_P,      U_a_vXYpOFFSET,  U_a_P,
/* 9 */ U_a_P,		 U_a_P,		  U_a_P,	   U_a_P,	    U_a_P,	     U_a_P,	      U_a_vXYpOFFSET,	  U_a_P,	   U_a_P,      U_a_P,	   U_a_P,	U_a_P,	      U_a_P,	  U_a_P,      U_a_vXYpOFFSET,  U_a_P,
/* A */ U_a_P,		 U_a_P,		  U_a_P,	   U_a_P,	    U_a_P,	     U_a_P,	      U_a_vXYpOFFSET,	  U_a_P,	   U_a_P,      U_a_P,	   U_a_P,	U_a_P,	      U_a_P,	  U_a_P,      U_a_vXYpOFFSET,  U_a_P,
/* B */ U_a_P,		 U_a_P,		  U_a_P,	   U_a_P,	    U_a_P,	     U_a_P,	      U_a_vXYpOFFSET,	  U_a_P,	   U_a_P,      U_a_P,	   U_a_P,	U_a_P,	      U_a_P,	  U_a_P,      U_a_vXYpOFFSET,  U_a_P,
/* C */ xy_illegal,	 xy_illegal,	  xy_illegal,	   xy_illegal,	    xy_illegal,	     xy_illegal,      xy_illegal,	  xy_illegal,	   xy_illegal, xy_illegal, xy_illegal,	xy_cb_prefix, xy_illegal, xy_illegal, xy_illegal,      xy_illegal,
/* D */ xy_illegal,	 xy_illegal,	  xy_illegal,	   xy_illegal,	    xy_illegal,	     xy_illegal,      xy_illegal,	  xy_illegal,	   xy_illegal, xy_illegal, xy_illegal,	xy_illegal,   xy_illegal, xy_xy,      xy_illegal,      xy_illegal,
/* E */ xy_illegal,	 pop_XY,	  xy_illegal,	   ex_vsp_XY,	    xy_illegal,	     push_XY,	      xy_illegal,	  xy_illegal,	   xy_illegal, jp_XY,	   xy_illegal,	xy_illegal,   xy_illegal, xy_illegal, xy_illegal,      xy_illegal,
/* F */ xy_illegal,	 xy_illegal,	  xy_illegal,	   xy_illegal,	    xy_illegal,	     xy_illegal,      xy_illegal,	  xy_illegal,	   xy_illegal, ld_sp_XY,   xy_illegal,	xy_illegal,   xy_illegal, xy_xy,      xy_illegal,      xy_illegal};

static Insn const xy_cb_insn_table[256] = {
/*	0		  1		    2		      3			4		  5		    6		      7			8		  9		    A		      B			C		  D		    E		      F */
/* 0 */ G_vXYpOFFSET_K,	  G_vXYpOFFSET_K,   G_vXYpOFFSET_K,   G_vXYpOFFSET_K,	G_vXYpOFFSET_K,	  G_vXYpOFFSET_K,   G_vXYpOFFSET,     G_vXYpOFFSET_K,	G_vXYpOFFSET_K,	  G_vXYpOFFSET_K,   G_vXYpOFFSET_K,   G_vXYpOFFSET_K,	G_vXYpOFFSET_K,	  G_vXYpOFFSET_K,   G_vXYpOFFSET,     G_vXYpOFFSET_K,
/* 1 */ G_vXYpOFFSET_K,	  G_vXYpOFFSET_K,   G_vXYpOFFSET_K,   G_vXYpOFFSET_K,	G_vXYpOFFSET_K,	  G_vXYpOFFSET_K,   G_vXYpOFFSET,     G_vXYpOFFSET_K,	G_vXYpOFFSET_K,	  G_vXYpOFFSET_K,   G_vXYpOFFSET_K,   G_vXYpOFFSET_K,	G_vXYpOFFSET_K,	  G_vXYpOFFSET_K,   G_vXYpOFFSET,     G_vXYpOFFSET_K,
/* 2 */ G_vXYpOFFSET_K,	  G_vXYpOFFSET_K,   G_vXYpOFFSET_K,   G_vXYpOFFSET_K,	G_vXYpOFFSET_K,	  G_vXYpOFFSET_K,   G_vXYpOFFSET,     G_vXYpOFFSET_K,	G_vXYpOFFSET_K,	  G_vXYpOFFSET_K,   G_vXYpOFFSET_K,   G_vXYpOFFSET_K,	G_vXYpOFFSET_K,	  G_vXYpOFFSET_K,   G_vXYpOFFSET,     G_vXYpOFFSET_K,
/* 3 */ G_vXYpOFFSET_K,	  G_vXYpOFFSET_K,   G_vXYpOFFSET_K,   G_vXYpOFFSET_K,	G_vXYpOFFSET_K,	  G_vXYpOFFSET_K,   G_vXYpOFFSET,     G_vXYpOFFSET_K,	G_vXYpOFFSET_K,	  G_vXYpOFFSET_K,   G_vXYpOFFSET_K,   G_vXYpOFFSET_K,	G_vXYpOFFSET_K,	  G_vXYpOFFSET_K,   G_vXYpOFFSET,     G_vXYpOFFSET_K,
/* 4 */ bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET,
/* 5 */ bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET,
/* 6 */ bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET,
/* 7 */ bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET, bit_N_vXYpOFFSET,
/* 8 */ M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET,   M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET,   M_N_vXYpOFFSET_K,
/* 9 */ M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET,   M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET,   M_N_vXYpOFFSET_K,
/* A */ M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET,   M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET,   M_N_vXYpOFFSET_K,
/* B */ M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET,   M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET,   M_N_vXYpOFFSET_K,
/* C */ M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET,   M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET,   M_N_vXYpOFFSET_K,
/* D */ M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET,   M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET,   M_N_vXYpOFFSET_K,
/* E */ M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET,   M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET,   M_N_vXYpOFFSET_K,
/* F */ M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET,   M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET_K, M_N_vXYpOFFSET,   M_N_vXYpOFFSET_K};


Z80 *init_cpu(size_t ram_size) {
    // Creates new cpu
    Z80 *new_z80 = (Z80*)calloc(1, sizeof(Z80));

    // Allocate memory for system RAM and ROM
    uint8_t *memory = malloc(sizeof(uint8_t) * ram_size);
    new_z80->memory = memory;

    generate_parity_table();

    return new_z80;
}

void execute_cycle(Z80 *cpu) {
    uint8_t opcode = FETCH(cpu, PC);
    insn_table[opcode](cpu);
}
