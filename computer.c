#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>
#include "computer.h"
#undef mips            /* gcc already has a def for mips */

unsigned int endianSwap(unsigned int);

void PrintInfo (int changedReg, int changedMem);
unsigned int Fetch (int);
void Decode (unsigned int, DecodedInstr*, RegVals*);
int Execute (DecodedInstr*, RegVals*);
int Mem(DecodedInstr*, int, int *);
void RegWrite(DecodedInstr*, int, int *);
void UpdatePC(DecodedInstr*, int);
void PrintInstruction (DecodedInstr*);

/*Globally accessible Computer variable*/
Computer mips;
RegVals rVals;

/*
 *  Return an initialized computer with the stack pointer set to the
 *  address of the end of data memory, the remaining registers initialized
 *  to zero, and the instructions read from the given file.
 *  The other arguments govern how the program interacts with the user.
 */
void InitComputer (FILE* filein, int printingRegisters, int printingMemory,
                   int debugging, int interactive) {
    int k;
    unsigned int instr;
    
    /* Initialize registers and memory */
    
    for (k=0; k<32; k++) {
        mips.registers[k] = 0;
    }
    
    /* stack pointer - Initialize to highest address of data segment */
    mips.registers[29] = 0x00400000 + (MAXNUMINSTRS+MAXNUMDATA)*4;
    
    for (k=0; k<MAXNUMINSTRS+MAXNUMDATA; k++) {
        mips.memory[k] = 0;
    }
    
    k = 0;
    while (fread(&instr, 4, 1, filein)) {
        /*swap to big endian, convert to host byte order. Ignore this.*/
        mips.memory[k] = ntohl(endianSwap(instr));
        k++;
        if (k>MAXNUMINSTRS) {
            fprintf (stderr, "Program too big.\n");
            exit (1);
        }
    }
    
    mips.printingRegisters = printingRegisters;
    mips.printingMemory = printingMemory;
    mips.interactive = interactive;
    mips.debugging = debugging;
}

unsigned int endianSwap(unsigned int i) {
    return (i>>24)|(i>>8&0x0000ff00)|(i<<8&0x00ff0000)|(i<<24);
}

/*
 *  Run the simulation.
 */
void Simulate () {
    char s[40];  /* used for handling interactive input */
    unsigned int instr;
    int changedReg=-1, changedMem=-1, val;
    DecodedInstr d;
    
    /* Initialize the PC to the start of the code section */
    mips.pc = 0x00400000;
    while (1) {
        if (mips.interactive) {
            printf ("> ");
            fgets (s,sizeof(s),stdin);
            if (s[0] == 'q') {
                return;
            }
        }
        
        /* Fetch instr at mips.pc, returning it in instr */
        instr = Fetch (mips.pc);
        
        printf ("Executing instruction at %8.8x: %8.8x\n", mips.pc, instr);
        
        /*
         * Decode instr, putting decoded instr in d
         * Note that we reuse the d struct for each instruction.
         */
        Decode (instr, &d, &rVals);
        
        /*Print decoded instruction*/
        PrintInstruction(&d);
        
        /*
         * Perform computation needed to execute d, returning computed value
         * in val
         */
        val = Execute(&d, &rVals);
        
        UpdatePC(&d,val);
        
        /*
         * Perform memory load or store. Place the
         * address of any updated memory in *changedMem,
         * otherwise put -1 in *changedMem.
         * Return any memory value that is read, otherwise return -1.
         */
        val = Mem(&d, val, &changedMem);
        
        /*
         * Write back to register. If the instruction modified a register--
         * (including jal, which modifies $ra) --
         * put the index of the modified register in *changedReg,
         * otherwise put -1 in *changedReg.
         */
        RegWrite(&d, val, &changedReg);
        
        PrintInfo (changedReg, changedMem);
    }
}

/*
 *  Print relevant information about the state of the computer.
 *  changedReg is the index of the register changed by the instruction
 *  being simulated, otherwise -1.
 *  changedMem is the address of the memory location changed by the
 *  simulated instruction, otherwise -1.
 *  Previously initialized flags indicate whether to print all the
 *  registers or just the one that changed, and whether to print
 *  all the nonzero memory or just the memory location that changed.
 */
void PrintInfo ( int changedReg, int changedMem) {
    int k, addr;
    printf ("New pc = %8.8x\n", mips.pc);
    if (!mips.printingRegisters && changedReg == -1) {
        printf ("No register was updated.\n");
    } else if (!mips.printingRegisters) {
        printf ("Updated r%2.2d to %8.8x\n",
                changedReg, mips.registers[changedReg]);
    } else {
        for (k=0; k<32; k++) {
            printf ("r%2.2d: %8.8x  ", k, mips.registers[k]);
            if ((k+1)%4 == 0) {
                printf ("\n");
            }
        }
    }
    if (!mips.printingMemory && changedMem == -1) {
        printf ("No memory location was updated.\n");
    } else if (!mips.printingMemory) {
        printf ("Updated memory at address %8.8x to %8.8x\n",
                changedMem, Fetch (changedMem));
    } else {
        printf ("Nonzero memory\n");
        printf ("ADDR      CONTENTS\n");
        for (addr = 0x00400000+4*MAXNUMINSTRS;
             addr < 0x00400000+4*(MAXNUMINSTRS+MAXNUMDATA);
             addr = addr+4) {
            if (Fetch (addr) != 0) {
                printf ("%8.8x  %8.8x\n", addr, Fetch (addr));
            }
        }
    }
}

/*
 *  Return the contents of memory at the given address. Simulates
 *  instruction fetch.
 */
unsigned int Fetch ( int addr) {
    return mips.memory[(addr-0x00400000)/4];
}

/* Decode instr, returning decoded instruction. */
void Decode ( unsigned int instr, DecodedInstr* d, RegVals* rVals) {
    /* Your code goes here */
    
    //Decode Instruction
    
    //Find opcode (26th - 32nd bit)... we use AND operator to isolate first 6 bits
    int opcode = (instr >> 26) & 0x3f;
    d->op = opcode;
    
    
    
    if (opcode == 0){ //Identify format based on opcode use InstrType Enum
        d->type = 0; //R Instruction
        
        d->regs.r.rs = (instr >> 21) & 0x1f; //Isolate first 5 bits
        d->regs.r.rt = (instr >> 16) & 0x1f;
        d->regs.r.rd = (instr >> 11) & 0x1f;
        d->regs.r.shamt = (instr >> 6) & 0x1f;
        int funct = instr & 0x3f;
        d->regs.r.funct = funct; //6 bits
        
        
        //Register Reads
        rVals->R_rs = mips.registers[d->regs.r.rs];
        rVals->R_rt = mips.registers[d->regs.r.rt];
        rVals->R_rd = mips.registers[d->regs.r.rd];
        
    } else if(opcode == 2 || opcode == 3){
        d->type = 2; //J Instruction
        d->regs.j.target = ((instr & 0x3ffffff) << 2) & 0xfffffff;
        
        
    } else {
        d->type = 1; //I instruction
        
        d->regs.i.rs = (instr >> 21) & 0x1f; //Isloate 5 bits
        d->regs.i.rt = (instr >> 16) & 0x1f;
        
        
        //Is this the properly extended version?
        d->regs.i.addr_or_immed = instr & 0x0000FFFF; //First 16 bits
        
        if (d->op == 4 || d->op == 5){
            d->regs.i.addr_or_immed = ((d->regs.i.addr_or_immed << 2) & 0xfffffff) + mips.pc + 4;
        } else {
            //Do sign extension
            unsigned int leftmost = (d->regs.i.addr_or_immed >> 15) & 1; //isolate single bit
            if (leftmost == 1){
                d->regs.i.addr_or_immed = d->regs.i.addr_or_immed + 0xFFFF0000;
            }
        }
        
        //Register Reads
        rVals->R_rs = mips.registers[d->regs.i.rs];
        rVals->R_rt = mips.registers[d->regs.i.rt];
        
        //Check for exit function
        if (opcode == 8){
            if(rVals->R_rs == 0 && rVals->R_rt == 0 && d->regs.i.addr_or_immed == 0){
                exit(0);
            }
        }
        
    }
    
}

/*
 *  Print the disassembled version of the given instruction
 *  followed by a newline.
 */
void PrintInstruction ( DecodedInstr* d) {
    /* Your code goes here */
    if (d->op == 0){
        switch (d->regs.r.funct){
            case 0x21: //addu
                printf("addu\t");
                break;
                
            case 0x23: //subu
                printf("subu\t");
                break;
                
            case 0x0: //sll
                printf("sll\t");
                break;
                
            case 0x2: //srl
                printf("srl\t");
                break;
                
            case 0x24: //and
                printf("and\t");
                break;
                
            case 0x25: //or
                printf("or\t");
                break;
                
            case 0x2a: //slt
                printf("slt\t");
                break;
                
            case 0x8: //jr
                printf("jr\t");
                break;
                
            default:
                exit(0);
        }
    } else if(d->op == 2){
        printf("j\t");
    } else if(d->op == 3){
        printf("jal\t");
    } else {
        switch(d->op){
            case 9: //addiu
                printf("addiu\t");
                break;
                
            case 8: //andi
                printf("andi\t");
                break;
                
            case 13: //ori
                printf("ori\t");
                break;
                
            case 15: //lui
                printf("lui\t");
                break;
                
            case 4: //beq
                printf("beq\t");
                break;
                
            case 5: //bne
                printf("bne\t");
                break;
                
            case 35: //lw
                printf("lw\t");
                break;
                
            case 43: //sw
                printf("sw\t");
                break;
                
            default: //bad instruction
                exit(0);
                break;
        }
    }
    
    if (d->type == 0){ //R instruction
        if (d->regs.r.funct == 0x8){ //jr
            printf("$%d\n", d->regs.r.rs);
        } else if (d->regs.r.funct == 0x0 || d->regs.r.funct == 0x2){ //sll
            printf("$%d, $%d, %d\n", d->regs.r.rd, d->regs.r.rt, d->regs.r.shamt);
        } else { //add
            printf("$%d, $%d, $%d\n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt);
        }
    } else if (d->type == 1){ //I instruction
        if (d->op == 35 || d->op == 43){ //lw and store word
            printf("$%d, %d($%d)\n", d->regs.i.rt, d->regs.i.addr_or_immed, d->regs.i.rs);
        } else if(d->op == 4 || d->op == 5){ //branch
            printf("$%d, $%d, 0x00%x\n", d->regs.i.rs, d->regs.i.rt, d->regs.i.addr_or_immed);
        } else if(d->op == 15 || d->op == 12 || d->op == 13){ //andi, ori, lui
            printf("$%d, $%d, 0x00%x\n", d->regs.i.rt, d->regs.i.rs, d->regs.i.addr_or_immed);
        } else{ //addi
            printf("$%d, $%d, %d\n", d->regs.i.rt, d->regs.i.rs, d->regs.i.addr_or_immed);
        }
    } else if (d->type == 2){ //J instruction
        printf("0x00%x\n", d->regs.j.target);
    }
}

/* Perform computation needed to execute d, returning computed value */
int Execute (DecodedInstr* d, RegVals* rVals) {
    /* Your code goes here */
    if (d->op == 0){
        switch (d->regs.r.funct){
            case 0x21: //addu
                return (unsigned)(rVals->R_rs + rVals->R_rt);
                break;
                
            case 0x23: //subu
                return (unsigned)(rVals->R_rs - rVals->R_rt);
                break;
                
            case 0x0: //sll
                return (rVals->R_rt << d->regs.r.shamt);
                break;
                
            case 0x2: //srl
                return (rVals->R_rt >> d->regs.r.shamt);
                break;
                
            case 0x24: //and
                return (rVals->R_rt & rVals->R_rs);
                break;
                
            case 0x25: //or
                return (rVals->R_rt | rVals->R_rs);
                break;
                
            case 0x2a: //slt
                return (int)(rVals->R_rt < rVals->R_rs);
                break;
                
            case 0x8: //jr
                return rVals->R_rs;
                break;
                
            default:
                exit(0);
        }
    } else if(d->op == 2){ //j (return address for UpdatePC)
        return d->regs.j.target;
    } else if(d->op == 3){ //jal (return address for UpdatePC)
        return d->regs.j.target;
    } else {
        int shifted;
        switch(d->op){
            case 9: //addiu
                //return (unsigned)(rVals->R_rs + d->regs.i.addr_or_immed);
                return (rVals->R_rs + d->regs.i.addr_or_immed);
                break;
                
            case 8: //andi
                return (rVals->R_rs & d->regs.i.addr_or_immed);
                break;
                
            case 13: //ori
                return (rVals->R_rs | d->regs.i.addr_or_immed);
                break;
                
            case 15: //lui
                //int shifted
                shifted = (d->regs.i.addr_or_immed << 16);
                return shifted;
                break;
                
            case 4: //beq
                if(rVals->R_rs == rVals->R_rt){ //return branch address
                    return d->regs.i.addr_or_immed;
                } else{
                    return 0;
                }
                break;
                
            case 5: //bne
                if(rVals->R_rs == rVals->R_rt){ //return branch address
                    return 0; //0 means dont add to the branch address
                } else{
                    return d->regs.i.addr_or_immed;
                }
                break;
                
            case 35: //lw
                return rVals->R_rs + d->regs.i.addr_or_immed;
                break;
                
            case 43: //sw
                return rVals->R_rs + d->regs.i.addr_or_immed;
                break;
                
            default: //bad instruction
                exit(0);
                break;
        }
    }
    
    return 0;
}

/*
 * Update the program counter based on the current instruction. For
 * instructions other than branches and jumps, for example, the PC
 * increments by 4 (which we have provided).
 */
void UpdatePC ( DecodedInstr* d, int val) {
    /* Your code goes here */
    // if (d->op == 2 || d->op == 3){ //jumps
    //     mips.pc = val;
    
    // } else //(d->op == 4 || d->op == 5){ //branches
    //     {
    //     mips.pc += val;
    //     }
    // mips.pc+=4;
    
    switch (d->op) {
        case 0:
            if (d->regs.r.funct == 8) {
                mips.pc = val;
            }
            else {
                mips.pc += 4;
            }
            break;
            
        case 2: //jump
            mips.pc = val;
            break;
            
        case 3: //jal (Similar to what was done in the if statements we made)
            mips.registers[31] = mips.pc + 4;
            mips.pc = val;
            break;
        
        case 4: //beq
            if (val != 0){
                mips.pc = val;
            } else {
                mips.pc += 4;
            }
            break;
            
        case 5: //bne
            if (val != 0){
                mips.pc = val;
            } else {
                mips.pc += 4;
            }
            break;
            
        default:
            mips.pc+=4;
    }
}

/*
 * Perform memory load or store. Place the address of any updated memory
 * in *changedMem, otherwise put -1 in *changedMem. Return any memory value
 * that is read, otherwise return -1.
 *
 * Remember that we're mapping MIPS addresses to indices in the mips.memory
 * array. mips.memory[0] corresponds with address 0x00400000, mips.memory[1]
 * with address 0x00400004, and so forth.
 *
 */
int Mem( DecodedInstr* d, int val, int *changedMem) {
    /* Your code goes here */
    if (d->op == 35){ //lw
        if (val >= MAXNUMINSTRS+MAXNUMDATA){
            printf("Memory Access Exception at %d: address %x", mips.pc, val);
            exit(0);
            return -1;
        }
        
        int loaded_word = mips.memory[val];
        *changedMem = -1;
        return loaded_word;
    } else if(d->op == 43){//sw
        
        if (val >= MAXNUMINSTRS+MAXNUMDATA){
            printf("Memory Access Exception at %d: address %x", mips.pc, val);
            exit(0);
            return -1;
        }
        mips.memory[val] = mips.registers[d->regs.i.rt];
        *changedMem = val;
        return -1;
    } else {
        *changedMem = -1;
        return val;
    }
}

/*
 * Write back to register. If the instruction modified a register--
 * (including jal, which modifies $ra) --
 * put the index of the modified register in *changedReg,
 * otherwise put -1 in *changedReg.
 */;

void RegWrite( DecodedInstr* d, int val, int *changedReg) {
    /* Your code goes here */
    if (val != -1){ //If we do something with registers
        if (d->type == 0){ //R instr
            if (d->regs.r.funct == 8){
                *changedReg = -1;
            } else {
                mips.registers[d->regs.r.rd] = val;
                *changedReg = d->regs.r.rd;
            }
            
        } else if (d->type == 1){ //I intsr
            if(d->op != 4 && d->op != 5){
                mips.registers[d->regs.i.rt] = val;
                *changedReg = d->regs.r.rt;
            } else{
                *changedReg = -1;
            }
        } else if (d->type == 2){ //J instr
             if (d->op == 3){
                 mips.registers[31] = val - 4;
                 *changedReg = 31;
             } else {
                 *changedReg = -1;
             }
        }
    }
}
