#pragma once

#include "ir/basic_block.h"
#include "ir/instruction.h"
#include "ir/ir.h"

#include <memory>
#include <ranges>
#include <unordered_map>
#include <vector>

namespace soul::ir
{
	/**
	 * @brief IRBuilder is a helper class, which simplifies building of IRs.
	 * @warning It shouldn't be used for modifying existing IRs.
	 */
	class IRBuilder
	{
		private:
		using StackSlotsMapping = std::unordered_map<std::string_view, StackSlot*>;

		private:
		std::unique_ptr<Module> _module{};
		BasicBlock*             _current_block{};
		BasicBlock::Label       _next_block_version{ 0 };
		Instruction::Version    _next_instruction_version{ 0 };
		StackSlotsMapping       _slots_mapping;

		public:
		constexpr IRBuilder();
		constexpr IRBuilder(const IRBuilder&)     = delete;
		constexpr IRBuilder(IRBuilder&&) noexcept = default;
		constexpr ~IRBuilder()                    = default;

		constexpr IRBuilder& operator=(const IRBuilder&)     = delete;
		constexpr IRBuilder& operator=(IRBuilder&&) noexcept = default;

		constexpr std::unique_ptr<Module> build() noexcept;

		constexpr void set_module_name(std::string_view name);

		/**
		 * @brief Creates a new function in the module (with a single basic block initialized).
		 * @warning Switches the current basic block to a newly initialized one.
		 */
		constexpr void       create_function(std::string_view         identifier,
		                                     types::Type              return_type,
		                                     std::vector<types::Type> parameters);
		constexpr StackSlot* reserve_slot(std::string_view identifier, types::Type type);
		constexpr StackSlot* get_slot(std::string_view identifier);

		constexpr void        switch_to(BasicBlock* block);
		constexpr BasicBlock* create_basic_block();
		constexpr BasicBlock* current_basic_block() const noexcept { return _current_block; }

		constexpr void connect(const std::ranges::forward_range auto& predecessors, BasicBlock* successor);
		constexpr void connect(BasicBlock* predecessor, const std::ranges::forward_range auto& successors);
		constexpr void connect(BasicBlock* predecessor, BasicBlock* successor);

		/**
		 * @brief Constructs new Instruction and appends it to the end of the current BasicBlock.
		 * @warning Cannot be used with Upsilon/Phi instructions -> use emit_upsilon/phi instead.
		 * @tparam Inst Instruction to be emitted.
		 * @tparam Args Arguments used to construct the Instruction.
		 * @return Pointer to the instruction emitted.
		 */
		template <InstructionKind Inst, typename... Args>
			requires(!(std::is_same_v<Inst, Upsilon> || std::is_same_v<Inst, Phi>))
		constexpr Instruction* emit(Args&&... args);

		/**
		 * @brief Constructs new Upsilon instruction and appends it to the end of the current BasicBlock.
		 * Associates the Upsilon with a given \p identifier.
		 * @param identifier Identifier to associate with this Upsilon.
		 * @tparam Args Arguments used to construct the Upsilon.
		 * @return Pointer to the instruction emitted.
		 */
		template <typename... Args>
		constexpr Instruction* emit_upsilon(std::string_view identifier, Args&&... args);

		/**
		 * @brief Constructs new Phi instruction and appends it to the end of the current BasicBlock.
		 * Patches all Upsilons associated with \p identifier up until this point.
		 * @param identifier Identifier to associate with this Phi.
		 * @tparam Args Arguments used to construct the Phi.
		 * @return Pointer to the instruction emitted.
		 */
		template <typename... Args>
		constexpr Instruction* emit_phi(std::string_view identifier, Args&&... args);

		private:
		template <InstructionKind Inst, typename... Args>
			requires(std::is_constructible_v<Inst, std::remove_cvref_t<Args>...>)
		constexpr Instruction* emit_impl(Args&&... args);
	};
}  // namespace soul::ir
#include "ir/builder.inl"
