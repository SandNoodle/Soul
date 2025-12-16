#include "IR/visitors/print.h"

#include "IR/instruction.h"
#include "IR/ir.h"

#include <format>
#include <ranges>

namespace Soul::ir::visitors
{
	using namespace std::string_view_literals;

	static constexpr auto k_nullptr   = "__nullptr__"sv;
	static constexpr auto k_separator = ", "sv;

	std::string PrintVisitor::string() const { return _ss.str(); }

	void PrintVisitor::accept(const Module& module)
	{
		_ss << std::format("module @{}\n\n", module.name);
		for (const auto& function : module.functions) {
			if (!function) {
				_ss << std::format("@fn {}() :: {} {{}}", k_nullptr, std::string(types::Type{}));
				continue;
			}

			_ss << std::format("fn @{}", function->name);
			_ss << "(";
			for (std::size_t parameter_index = 0; parameter_index < function->parameters.size(); ++parameter_index) {
				_ss << std::format("%{}::{}", parameter_index, std::string(function->parameters[parameter_index]));
				if (parameter_index != function->parameters.size() - 1) {
					_ss << k_separator;
				}
			}
			_ss << ")";
			_ss << std::format(" :: {} {{\n", std::string(function->return_type));

			for (const auto& basic_block : function->basic_blocks) {
				if (!basic_block) {
					_ss << std::format("\t#{}:", k_nullptr);
					continue;
				}
				_ss << std::format("\t#{}:\n", basic_block->label);
				for (const auto& instruction : basic_block->instructions) {
					if (!instruction) {
						_ss << k_nullptr;
						continue;
					}
					_ss << std::format("\t\t%{} = ", instruction->version);
#define SOUL_INSTRUCTION(name)          \
	if (instruction->is<name>()) {      \
		visit(instruction->as<name>()); \
	}
					SOUL_ALL_INSTRUCTIONS
#undef SOUL_INSTRUCTION
					_ss << std::format(" :: {}\n", std::string(instruction->type));
				}
				_ss << "\t\t; successors: [";
				for (auto successor = std::begin(basic_block->successors);
				     successor != std::end(basic_block->successors);
				     ++successor) {
					_ss << std::format("#{}", *successor ? std::to_string((*successor)->label) : k_nullptr);
					if (std::next(successor) != basic_block->successors.end()) {
						_ss << k_separator;
					}
				}
				_ss << "]\n";
			}
			_ss << "}\n";
		}
	}

	void PrintVisitor::visit(const Unreachable&) { _ss << "Unreachable()"sv; }

	void PrintVisitor::visit(const Noop&) { _ss << "Noop()"sv; }

	void PrintVisitor::visit(const Cast& instruction)
	{
		_ss << std::format("Cast(%{})",
		                   instruction.args[0] ? instruction.args[0]->version : Instruction::k_invalid_version);
	}

	void PrintVisitor::visit(const Call& instruction)
	{
		auto parameters{ instruction.parameters  //
			             | std::views::transform(
							 [](const auto& p) { return p ? p->version : Instruction::k_invalid_version; })       //
			             | std::views::transform([](const auto version) { return std::format("%{}", version); })  //
			             | std::views::join_with(k_separator)                                                     //
			             | std::ranges::to<std::string>() };
		_ss << std::format("Call(`{}`, [{}])", instruction.identifier, parameters);
	}

	void PrintVisitor::visit(const Const& instruction)
	{
		_ss << std::format("Const(`{}`)", std::string(instruction.value));
	}

	void PrintVisitor::visit(const Jump& instruction)
	{
		_ss << std::format("Jump(#{})", instruction.target ? std::to_string(instruction.target->label) : k_nullptr);
	}

	void PrintVisitor::visit(const JumpIf& instruction)
	{
		_ss << std::format("JumpIf(%{}, #{}, #{})",
		                   instruction.args[0] ? instruction.args[0]->version : Instruction::k_invalid_version,
		                   instruction.then_block ? instruction.then_block->label : Instruction::k_invalid_version,
		                   instruction.else_block ? instruction.else_block->label : Instruction::k_invalid_version);
	}

	void PrintVisitor::visit(const Return& instruction)
	{
		_ss << std::format("Return(%{})",
		                   instruction.args[0] ? instruction.args[0]->version : Instruction::k_invalid_version);
	}

	void PrintVisitor::visit([[maybe_unused]] const StackSlot& instruction) { _ss << std::format("StackSlot()"); }

	void PrintVisitor::visit(const StackStore& instruction)
	{
		_ss << std::format("StackStore(%{}, %{})",
		                   instruction.slot ? instruction.slot->version : Instruction::k_invalid_version,
		                   instruction.args[0] ? instruction.args[0]->version : Instruction::k_invalid_version);
	}

	void PrintVisitor::visit(const StackLoad& instruction)
	{
		_ss << std::format("StackLoad(%{})",
		                   instruction.slot ? instruction.slot->version : Instruction::k_invalid_version);
	}

	void PrintVisitor::visit(const GetArgument& instruction)
	{
		_ss << std::format("GetArgument({})", instruction.index);
	}

	void PrintVisitor::visit([[maybe_unused]] const Phi& instruction) { _ss << "Phi()"sv; }

	void PrintVisitor::visit(const Upsilon& instruction)
	{
		_ss << std::format("Upsilon(%{}, ^%{})",
		                   instruction.args[0] ? instruction.args[0]->version : Instruction::k_invalid_version,
		                   k_nullptr);
	}

	void PrintVisitor::visit(const Not& instruction)
	{
		_ss << std::format("Not(%{})",
		                   instruction.args[0] ? instruction.args[0]->version : Instruction::k_invalid_version);
	}

#define SOUL_INSTRUCTION(name)                                                                                   \
	void PrintVisitor::visit(const name& instruction)                                                            \
	{                                                                                                            \
		_ss << std::format("{}(%{}, %{})",                                                                       \
		                   #name,                                                                                \
		                   instruction.args[0] ? instruction.args[0]->version : Instruction::k_invalid_version,  \
		                   instruction.args[1] ? instruction.args[1]->version : Instruction::k_invalid_version); \
	}
	SOUL_ARITHMETIC_INSTRUCTIONS
	SOUL_COMPARISON_INSTRUCTIONS
	SOUL_LOGICAL_INSTRUCTIONS
#undef SOUL_INSTRUCTION

}  // namespace soul::ir::visitors
