#pragma once

#include "IR/BasicBlock.h"
#include "IR/IR.h"
#include "IR/Instruction.h"

#include <memory>
#include <unordered_map>
#include <vector>

namespace Soul::IR
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
		BasicBlock* _current_block{};
		BasicBlock::Label _next_block_version{ 0 };
		Instruction::Version _next_instruction_version{ 0 };
		StackSlotsMapping _slots_mapping{};

		public:
		constexpr IRBuilder();
		IRBuilder(const IRBuilder&)     = delete;
		IRBuilder(IRBuilder&&) noexcept = default;
		~IRBuilder()                    = default;

		IRBuilder& operator=(const IRBuilder&)     = delete;
		IRBuilder& operator=(IRBuilder&&) noexcept = default;

		constexpr std::unique_ptr<Module> build() noexcept;

		constexpr void set_module_name(std::string_view name);

		/**
		 * @brief Creates a new function in the module (with a single basic block initialized).
		 * @warning Switches the current basic block to a newly initialized one.
		 */
		constexpr void create_function(std::string_view identifier,
		                               Types::Type return_type,
		                               std::vector<Types::Type> parameters);
		constexpr StackSlot* reserve_slot(std::string_view identifier, Types::Type type);
		constexpr StackSlot* get_slot(std::string_view identifier);

		constexpr void switch_to(BasicBlock* block);
		constexpr BasicBlock* create_basic_block();
		constexpr BasicBlock* current_basic_block() const noexcept;

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

		private:
		template <InstructionKind Inst, typename... Args>
			requires(std::is_constructible_v<Inst, std::remove_cvref_t<Args>...>)
		constexpr Instruction* emit_impl(Args&&... args);
	};
}  // namespace Soul::IR
#include "IR/Builder.inl"
