#pragma once

#include <cassert>

namespace Soul::IR
{
	constexpr IRBuilder::IRBuilder() : _module(std::make_unique<Module>("")) {}

	constexpr auto IRBuilder::Build() noexcept -> std::unique_ptr<Module>
	{
		// Patch basic block's successors.
		for (auto& function : _module->functions) {
			const auto emplace_if_unique = [](auto& container, BasicBlock* target) -> void {
				if (std::find(container.begin(), container.end(), target) != container.end()) {
					return;
				}
				container.emplace_back(target);
			};

			for (auto& basic_block : function->basic_blocks) {
				basic_block->successors.clear();
				for (auto& instruction : basic_block->instructions) {
					if (instruction->Is<Jump>()) {
						emplace_if_unique(basic_block->successors, instruction->As<Jump>().target);
					}
					if (instruction->Is<JumpIf>()) {
						emplace_if_unique(basic_block->successors, instruction->As<JumpIf>().then_block);
						emplace_if_unique(basic_block->successors, instruction->As<JumpIf>().else_block);
					}
				}

				std::sort(basic_block->successors.begin(),
				          basic_block->successors.end(),
				          [](const BasicBlock* lhs, const BasicBlock* rhs) -> bool { return lhs->label < rhs->label; });
			}
		}

		return std::move(_module);
	}

	constexpr auto IRBuilder::SetModuleName(std::string_view name) const -> void { _module->name = std::string(name); }

	constexpr auto IRBuilder::CreateFunction(std::string_view identifier,
	                                         Types::Type return_type,
	                                         std::vector<Types::Type> parameters) -> void
	{
		_next_instruction_version = 0;
		_module->functions.emplace_back(
			std::make_unique<Function>(identifier, std::move(return_type), std::move(parameters)));
		_current_block = CreateBasicBlock();
	}

	constexpr auto IRBuilder::ReserveSlot(std::string_view identifier, Types::Type type) -> StackSlot*
	{
		auto* slot                 = static_cast<StackSlot*>(Emit<StackSlot>(std::move(type)));
		_slots_mapping[identifier] = slot;
		return slot;
	}
	constexpr auto IRBuilder::GetSlot(std::string_view identifier) const -> StackSlot*
	{
		if (!_slots_mapping.contains(identifier)) [[unlikely]] {
			return nullptr;
		}
		return _slots_mapping.at(identifier);
	}

	constexpr auto IRBuilder::SwitchTo(BasicBlock* block) -> void
	{
		assert(block && "uninitialized block was passed (nullptr)");
		_current_block = block;
	}

	constexpr auto IRBuilder::CreateBasicBlock() -> BasicBlock*
	{
		auto& current_function = _module->functions.back();
		current_function->basic_blocks.emplace_back(std::make_unique<BasicBlock>(_next_block_version++));
		return current_function->basic_blocks.back().get();
	}

	constexpr auto IRBuilder::CurrentBasicBlock() const noexcept -> BasicBlock* { return _current_block; }

	template <InstructionKind Inst, typename... Args>
		requires(!(std::is_same_v<Inst, Upsilon> || std::is_same_v<Inst, Phi>))
	constexpr auto IRBuilder::Emit(Args&&... args) -> Instruction*
	{
		return EmitImpl<Inst, Args...>(std::forward<Args>(args)...);
	}

	template <InstructionKind Inst, typename... Args>
		requires(std::is_constructible_v<Inst, std::remove_cvref_t<Args>...>)
	constexpr auto IRBuilder::EmitImpl(Args&&... args) -> Instruction*
	{
		assert(_current_block && "_current_block was not initialized properly (nullptr)");
		assert(_current_block->label != BasicBlock::k_invalid_label && "_current_block is invalid (k_invalid_label)");
		_current_block->instructions.emplace_back(std::make_unique<Inst>(std::forward<Args>(args)...));
		_current_block->instructions.back()->version = _next_instruction_version++;
		return static_cast<Inst*>(_current_block->instructions.back().get());
	}
}  // namespace Soul::IR
