#include "vm.h"

soul_vm_t soul_vm_create(void)
{
	soul_vm_t vm;
	vm.ip = 0;
	return vm;
}
